use strict;
use FindBin;
use Test::More tests => 14;

my $file = "$FindBin::Bin/file.txt";

BEGIN { use_ok('Tk::DiffText') };


_load: {
	my $got;
	my $exp = ["foo\n", "bar\n", "baz\n"];
	my $fh;

	$got = Tk::DiffText::_load($file);
	is_deeply($got, $exp, q"_load('file')");

	$got = Tk::DiffText::_load(["foo\n", "bar\n", "baz\n"]);
	is_deeply($got, $exp, '_load([list])');

	$got = Tk::DiffText::_load("foo\nbar\nbaz\n");
	is_deeply($got, $exp, q"_load('string')");

	open(FH, "< $file");
	$got = Tk::DiffText::_load(*FH);
	is_deeply($got, $exp, '_load(*FH)');

	seek(FH, 0, 0);

	$got = Tk::DiffText::_load(\*FH);
	is_deeply($got, $exp, '_load(\*FH)');

	seek(FH, 0, 0);

	open($fh, "< $file");
	$got = Tk::DiffText::_load($fh);
	is_deeply($got, $exp, '_load($fh)');

	seek($fh, 0, 0);

	$got = Tk::DiffText::_load(\$fh);
	is_deeply($got, $exp, '_load(\$fh)');
	close($fh);

	SKIP: {
		eval {require IO::File};
		skip('IO::File not available', 5) if $@;
	
		$got = Tk::DiffText::_load(*FH{IO});
		is_deeply($got, $exp, '_load(*FH{IO})');
	
		seek(FH, 0, 0);
	
		$got = Tk::DiffText::_load(*FH{IO});
		is_deeply($got, $exp, '_load(\*FH{IO})');
		close(FH);
	
		{
			local $^W; # no warnings 'deprecated' would require perl 5.6

			open(FH, "< $file");
			$got = Tk::DiffText::_load(*FH{FILEHANDLE});
			is_deeply($got, $exp, '_load(*FH{FILEHANDLE})');
		
			seek(FH, 0, 0);
		
			$got = Tk::DiffText::_load(*FH{FILEHANDLE});
			is_deeply($got, $exp, '_load(\*FH{FILEHANDLE})');
		}

		close(FH);

		$fh = IO::File->new($file, 0);
		$got = Tk::DiffText::_load($fh);
		is_deeply($got, $exp, '_load(IO::File)');
		$fh->close;
	}
}

_sdiff: {
	SKIP: {
		eval {require Algorithm::Diff};
		skip('Algorithm::Diff not available', 1) if $@;

		my $a = ["foo\n", "bar\n",    "baz"];
		my $b = ["bar\n", "bazaar\n", "quux\n"];

		my $got = Tk::DiffText::_sdiff($a, $b);
	
		my $exp = [
			['-', \$a->[0],  undef  ],
			['u', \$a->[1], \$b->[0]],
			['c', \$a->[2], \$b->[1]],
			['+',  undef,   \$b->[2]],
		];
	
		is_deeply($got, $exp, '_sdiff()');
	}
}
