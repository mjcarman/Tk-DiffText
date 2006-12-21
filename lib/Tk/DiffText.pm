#===============================================================================
# Tk/DiffText.pm
# Last Modified: 11/14/2006 7:31PM
#===============================================================================
BEGIN {require 5.005} # for qr//
use strict;

use Tk;
use Tk::widgets qw'ROText Scrollbar';

package Tk::DiffText;
use vars qw'$VERSION';
$VERSION = '0.11';

use base qw'Tk::Frame';
Tk::Widget->Construct('DiffText');

my @tag = (
	{'u' => undef, 'c' => 'mod', '-' => 'del', '+' => 'del'},
	{'u' => undef, 'c' => 'mod', '-' => 'add', '+' => 'add'},
);


#-------------------------------------------------------------------------------
# Method  : ClassInit
# Purpose : Class initialization.
# Notes   : 
#-------------------------------------------------------------------------------
sub ClassInit {
	my ($class, $mw) = @_;
	$class->SUPER::ClassInit($mw);

	# This module is pretty crippled if Algorithm::Diff isn't available,
	# but let's degrade nicely instead of dieing.
	eval {require Algorithm::Diff};
	*_diff = $@ ? \&_show_plain_text : \&_show_diff_text;

	# Only needed to resolve passing data inputs as *FH{IO} or *FH{FILEHANDLE}
	eval {require IO::File};
}


#-------------------------------------------------------------------------------
# Method  : Populate
# Purpose : Create a DiffText composite widget.
# Notes   : 
#-------------------------------------------------------------------------------
sub Populate {
	my ($self, $arg) = @_;

	my $gutter = delete $arg->{-gutter};
	my $orient = delete $arg->{-orient};

	$gutter = 1          unless defined $gutter;
	$orient = 'vertical' unless defined $orient;

	# I'm not sure whether it's a bug of a feature, but Frames with contents
	# always collapse down to just what's needed by the widgets they contain.
	# This makes setting the width and height of the composite widget worthless.
	# Empty Frames, on the other hand, *do* respect height and width settings.
	# We exploit this by creating invisible frames along the top and left 
	# edges of the composite widget and using them to control its size.
	my $f  = $self;
	my $wf = $f->Frame(-height => 0, -borderwidth => 0)->pack(-side => 'top');
	my $hf = $f->Frame(-width  => 0, -borderwidth => 0)->pack(-side => 'left');

	my $diffcolors = {
		add    => [-background => '#ccffcc'],
		del    => [-background => '#ffcccc'],
		mod    => [-background => '#aed7ff'],
		pad    => [-background => '#f0f0f0'],
	};

	if ($arg->{-diffcolors}) {
		while (my ($k, $v) = each %{$arg->{-diffcolors}}) {
			$diffcolors->{$k} = $v;
		}
		delete $arg->{-diffcolors};
	}

	$self->SUPER::Populate($arg);

	my @fw; # frame widgets
	my @tw; # text widgets
	my @gw; # gutter widgets

	for (0 .. 1) {

		push @fw, $f->Frame()->pack(
			-side => $orient eq 'horizontal' ? 'top' : 'left',
			-fill => 'both',
			-expand => 1,
		);

		my $hsb = $fw[-1]->Scrollbar(-orient => 'horizontal')
			->pack(-side => 'bottom', -fill => 'x');

		push @gw, $fw[-1]->ROText(
			-height      => 1, # height fills to match text areas
			-width       => 1, # just for starters
			-borderwidth => 0,
			-state       => 'disabled',
			-wrap        => 'none',
		)->pack(-side => 'left', -fill => 'y');

		push @tw, $fw[-1]->ROText(
			-width          => 1, # size controlled via parent so that panes are
			-height         => 1, # always balanced even when window resized.
			-borderwidth    => 0,
			-xscrollcommand => ['set' => $hsb],
		)->pack(-side => 'left', -fill => 'both', -expand => 1);

		for my $w ($tw[-1]) {
			$w->tagConfigure('norm', @{[]});
			$w->tagConfigure('add',  @{$diffcolors->{add}});
			$w->tagConfigure('del',  @{$diffcolors->{del}});
			$w->tagConfigure('mod',  @{$diffcolors->{mod}});
			$w->tagConfigure('pad',  @{$diffcolors->{pad}});
			$w->tagRaise('sel');
		}

		my $t = $tw[-1]; # make static widget ref for scrollbar closure
		$hsb->configure(-command => sub { $t->xview(@_) });

	}

	my $sb = $f->Scrollbar()->pack(
		-before => $orient eq 'horizontal' ? $fw[0] : $fw[1],
		-side   => 'left',
		-fill   => 'y',
	);


	my @slw;
	if ($gutter) {
		@slw = (@tw, @gw);
	}
	else {
		@slw = @tw;
		$_->packForget() foreach @gw;
	}

	# scrollbar controls all scroll-locked widgets
	$sb->configure(-command => sub { $_->yview(@_) foreach (@slw) });

	# scrolling any scroll-locked widget scrolls all of them
	foreach my $x (@slw) {
		$x->configure(-yscrollcommand => [\&_scroll_panes, $sb, $x, \@slw]);
	}

	$self->ConfigSpecs(
		# overall widget size
		-width            => [{-width      => $wf }, qw'width      Width      780'],
		-height           => [{-height     => $hf }, qw'height     Height     580'],

		# aliases for controlling gutter configuration
		-gutterbackground => [{-background => \@gw}, qw'background Background', '#f0f0f0'],
		-gutterforeground => [{-foreground => \@gw}, qw'foreground Foreground', '#5c5c5c'],

		# We want the gutter to look like it's part of the main ROText widget, 
		# not a seperate one. To do that we set -borderwidth to 0 on the widgets 
		# and use the frames enclosing them to provide the borders.
		-relief           => [\@fw, qw'relief      Relief      sunken'],
		-borderwidth      => [\@fw, qw'borderwidth borderWidth 2'     ],

		# pass most options through to the ROText widgets.
		# Sometimes to just the text ones, sometimes to the gutters too.
		DEFAULT           => [\@tw],
		-background       => [\@tw], # DEFAULT doesn't catch fg/bg?
		-foreground       => [\@tw],
		-font             => [[@tw, @gw]], # sync gutter font to text for vertical alignment
		-pady             => [[@tw, @gw]], # pad gutter too for y, (valign) but not for x
		-wrap             => [\@tw, qw'wrap Wrap none'],

	);

	# Don't want to advertise these widgets (at least for now)
	# but we need access to them in method calls.
	$self->{_widget} = {
		text   => \@tw,
		gutter => \@gw,
	};

	# Advertising the scrollbar makes it possible to align widgets over the text panes
	# (like a Entry containing the file name) via a spacer frame with it's width
	# set to match the scrollbars. ($w->Subwidget('scrollbar')->cget('width'))
	$self->Advertise(
		scrollbar => $sb,
	);

#	$self->Delegates(
#		load_text0 => $tw[0],
#		load_text1 => $tw[1],
#	);

}


#-------------------------------------------------------------------------------
# Subroutine : _scroll_panes
# Purpose    : synchronize scrolling between panes
# Notes      :
#-------------------------------------------------------------------------------
sub _scroll_panes {
	my $sb = shift; # scrollbar
	my $w  = shift; # calling widget
	my $sw = shift; # scrolled widgets

	$sb->set(@_);
	my ($top, $bottom) = $w->yview();
	$_->yviewMoveto($top) foreach @$sw;
}


#-------------------------------------------------------------------------------
# Method  : diff
# Purpose : Load and compare files
# Notes   : 
#-------------------------------------------------------------------------------
sub diff {
	my ($self) = @_;
	my $tw = $self->{_widget}{text};
	my $gw = $self->{_widget}{gutter};

	$gw->[0]->configure(-state => 'normal');
	$gw->[1]->configure(-state => 'normal');
	$gw->[0]->delete('0.0', 'end');
	$gw->[1]->delete('0.0', 'end');
	$tw->[0]->delete('0.0', 'end');
	$tw->[1]->delete('0.0', 'end');

	$self->update();

	&_diff; # call helper w/same args

	$gw->[0]->configure(-state => 'disabled');
	$gw->[1]->configure(-state => 'disabled');

	$self->update();
}


#-------------------------------------------------------------------------------
# Subroutine : _show_plain_text
# Purpose    : show both files as plain text (no markup)
# Notes      :
#-------------------------------------------------------------------------------
sub _show_plain_text {
	my $self = shift;
#	my %opt  = @_[2 .. $#_]; # options unused for diff-less mode
	my $tw   = $self->{_widget}{text};
	my $gw   = $self->{_widget}{gutter};

	my $x  = _load($_[0]); # don't make copies of $_[0] and $_[1] in case
	my $y  = _load($_[1]); # they contain an entire slurped file.

	my $z  = @$y - @$x;
	my $w  = length($z);
	my @ln = (1, 1);

	$gw->[0]->configure(-width => $w + 1);
	$gw->[1]->configure(-width => $w + 1);

	foreach my $l (@$x) {
		$gw->[0]->insert('end', sprintf("%${w}i\n", $ln[0]++));
		$tw->[0]->insert('end', $l);
	}
	if ($z > 0) {
		for (1 .. $z) {
			$gw->[0]->insert('end', "\n");
			$tw->[0]->insert('end', "\n");
		}
	}

	foreach my $l (@$y) {
		$gw->[1]->insert('end', sprintf("%${w}i\n", $ln[1]++));
		$tw->[1]->insert('end', $l);
	}
	if ($z < 0) {
		for (1 .. -$z) {
			$gw->[1]->insert('end', "\n");
			$tw->[1]->insert('end', "\n");
		}
	}
}


#-------------------------------------------------------------------------------
# Subroutine : _show_diff_text
# Purpose    : show highlighted differences between files
# Notes      :
#-------------------------------------------------------------------------------
sub _show_diff_text {
	my $self  = shift;
	my %opt   = @_[2 .. $#_];
	my $tw    = $self->{_widget}{text};
	my $gw    = $self->{_widget}{gutter};

	my $x = _load($_[0]); # don't make copies of $_[0] and $_[1] in case
	my $y = _load($_[1]); # they contain an entire slurped file.

	my $kg   = _make_sdiff_keygen(%opt);
	my @diff = _sdiff($x, $y, $kg);
	my $w    = length(scalar @diff);

	$gw->[0]->configure(-width => $w + 1);
	$gw->[1]->configure(-width => $w + 1);

	my $re;
	for ($opt{-granularity}) {
		ref eq 'Regexp' && do { $re = $_;           last };
		/^line$/        && do { $re = undef;        last };
		/^word$/        && do { $re = qr/(\s+|\b)/; last };
		/^char$/        && do { $re = qr//;         last };
		! ref           && do { $re = qr/$_/;       last };
	}

	my @ln = (1,  1);
	foreach my $d (@diff) {

		if ($re && $d->[0] eq 'c') {
			# Provide detail on changes within the line.

			# Remove any trailing newline so that it won't cause the tag to
			# highlight to EOL.
			my @nl = (chomp ${$d->[1]}, chomp ${$d->[2]});
			my $dx = [split($re, ${$d->[1]})];
			my $dy = [split($re, ${$d->[2]})];
			my @dd = _sdiff($dx, $dy);

			foreach my $d (@dd) {
				$tw->[0]->insert('end', ${$d->[1]}, $tag[0]{$d->[0]});
				$tw->[1]->insert('end', ${$d->[2]}, $tag[1]{$d->[0]});
			}

			# Replace any newlines removed so that we know whether or
			# not to pad the line. Also write a newline to the text area.
			for my $i (0 .. 1) {
				next unless $nl[$i];
				${$d->[$i+1]} .= "\n";
				$tw->[$i]->insert('end', "\n");
			}

		}
		else {
			# Either the whole line matches, or it doesn't match at all (add/del)
			# or we don't want finer resolution on what the differences are.
			$tw->[0]->insert('end', ${$d->[1]}, $tag[0]{$d->[0]});
			$tw->[1]->insert('end', ${$d->[2]}, $tag[1]{$d->[0]});
		}

		for my $n (0 .. 1) {
			if (${$d->[$n+1]} =~ /\n/) {
				# Add line number from source file to gutter
				$gw->[$n]->insert('end', sprintf("%${w}i\n", $ln[$n]++));
			}
			else {
				# Pad text display to align matches
				# Leave line number empty
				$tw->[$n]->insert('end', "\n", 'pad');
				$gw->[$n]->insert('end', "\n");
			}
		}

	}
}


#-------------------------------------------------------------------------------
# Subroutine : _sdiff
# Purpose    : Replacement for Algorithm::Diff::sdiff that returns references
#              to sequences instead of copies of them. (This is to reduce memory
#              usage.)
# Notes      : 
#-------------------------------------------------------------------------------
sub _sdiff {
	my $a = shift;
	my $b = shift;
	my $d = [];

	Algorithm::Diff::traverse_balanced($a, $b,
		{
			MATCH     => sub { push @$d, ['u', \$a->[$_[0]], \$b->[$_[1]]] },
			DISCARD_A => sub { push @$d, ['-', \$a->[$_[0]],     undef   ] },
			DISCARD_B => sub { push @$d, ['+',     undef   , \$b->[$_[1]]] },
			CHANGE    => sub { push @$d, ['c', \$a->[$_[0]], \$b->[$_[1]]] },
		},
		@_
	);

	return wantarray ? @$d : $d;
}


#-------------------------------------------------------------------------------
# Subroutine : _make_sdiff_keygen
# Purpose    : Create a callback for tuning sdiff behavior based on options
# Notes      :
#-------------------------------------------------------------------------------
sub _make_sdiff_keygen {
	my %opt = 	(
		-whitespace => 1, # whitespace matters by default
		-case       => 1, # case matters by default
		@_
	);

	return sub {$_[0]} if ($opt{-case} && $opt{-whitespace});

	my $sub = 'sub { local $_ = $_[0]; ';
	$sub .= '$_ = lc $_; '                    if ! $opt{-case};
	$sub .= 's/^\s+//; s/\s+$//; tr/ \t/ /s;' if ! $opt{-whitespace};
	$sub .= 'return $_; }';

	return eval $sub;
}


#-------------------------------------------------------------------------------
# Subroutine : _load
# Purpose    : Load data from fine name, handle, scalar, whatever...
# Notes      :
#-------------------------------------------------------------------------------
sub _load {

	# Accept naive user input
	$_[0] = ${$_[0]} if ref $_[0] eq 'REF';  # \*FH{IO} instead of *FH{IO}
	$_[0] = *{$_[0]} if ref $_[0] eq 'GLOB'; # \*FH     instead of *FH

	if (ref $_[0]) {
		if (ref $_[0] eq 'ARRAY') {
			return $_[0]; # assume lines of file data, nothing to do, woohoo!
		}
		elsif ($_[0]->can('getlines')) {
			# IO::File must be loaded for this to work
			return [$_[0]->getlines]; # assume IO::File or equiv
		}
		else {
			warn sprintf("Don't know how to load from '%s' reference\n", ref $_[0]);
			return;
		}
	}
	elsif ($_[0] =~ /^\*(\w*::)+\$?\w+$/) {
		# GLOB; assume open filehandle
		my $fh = $_[0]; # copy to scalar so that <> interprets it as a filehandle
		return [<$fh>]; # and not a glob pattern. cf. perlop - I/O Operators
	}
	elsif ($_[0] =~ /\n/) {
		return [split /^/m, $_[0]] # assume contents of slurped file
	}		
	elsif (-T $_[0]) {
		# Need two-arg open() for perls < v5.6
		# what version added open($fh...) in place of open(FH...)
		local *FH;
		open(FH, "< $_[0]") or do { warn "Can't read file '$_[0]' [$!]\n"; return };
		my @data = <FH>;
		close(FH);
		return \@data;
	}
	else {
		warn sprintf("Don't know how to load data from '%s'\n", $_[0]);
		return;
	}
}


1;

__END__

=pod

=head1 NAME

Tk::DiffText - Perl/Tk composite widget for colorized diffs.

=head1 SYNOPSIS

  use Tk::DiffText;

  my $w = $mw->DiffText()->pack();

  $w->diff($file0, $file1);

=head1 DESCRIPTION

This module defines a composite widget that makes it simple to provide basic
"diff" functionality to your Tk applications.

=head1 OPTIONS

C<-orient =E<gt> 'horizontal'|'vertical'>

Controls the arrangement of the text panes. Defaults to B<vertical>.

C<-gutter =E<gt> 0|1>

Hides and displays the line number gutter. Defaults to B<1>.

C<-gutterforeground =E<gt> color>

Sets the gutter foreground color.

C<-gutterbackground =E<gt> color>

Sets the gutter background color.

C<-diffcolors =E<gt> {...}>

Sets the colors used for highlighting diff results. The structure of the
value hash is as follows:

  {
    add => [-fg => 'green'  ],               # tag for additions
    del => [-fg => 'red', -overstrike => 1], # tag for deletions
    mod => [-fg => 'blue'   ],               # tag for changes
    pad => [-bg => '#f0f0f0'],               # tag for blank line padding
  }

For each of the tags you can specify any option that is valid for use in a 
ROText widget tag: -foreground, -background, -overstrike, etc.

=head1 METHODS

=head2 C<diff>

  $w->diff(
  	$data1,
  	$data2,
  	-case        => 0,
  	-whitespace  => 0,
  	-granularity => 'line', # or 'word' 'char' or regexp
  );

Does a diff between C<$data1> and C<$data2> and loads the results into the text 
display. Normally these arguments are filenames but they can also be a reference 
to an array of lines of data, a string containing a slurped file, an open 
filehandle, a glob (which is interpreted as a filehandle), an IO::File object
or any other object with a C<getlines> method.

Setting either C<-case> or C<-whitespace> to 0 instructs the diff algorithm to 
ignore case and whitespace, respectively.

The C<-granularity> option controls the level of detail at which the diff is 
performed. The default value, 'line,' shows differences between lines. Changing 
it to 'word' or 'char' will show differences I<within> a line at the word or 
character level. You may also pass a C<qr//> quoted regular expression or a 
string which will be interpreted as a regular expression.

Note: For performance reasons, diffs are always performed line-by-line first. 
Finer granularity settings are only applied to lines marked as changed by the 
initial comparison. This can lead to slightly different results than you would 
get if you did a single diff at the higher level of resolution. (The results
aren't wrong, just different.)

=head1 BUGS

The API isn't settled yet. Comments welcome.

Some configuration settings (-gutter, -orient, -diffcolors) are only valid at 
creation time and cannot be changed later.

The line numbers in the gutter can get out of sync with the file display if you 
set -wrap to something other than 'none' (so don't do that).

=head1 AUTHOR

Michael J. Carman <mjcarman@mchsi.com>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2006 by Michael J. Carman

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
