use strict;
use warnings;

use Importer 'Test::More';

BEGIN {
    $INC{'My/Exporter.pm'} = 1;
    package My::Exporter;

    sub IMPORTER_MENU {
        return (
            export_anon => {
                a => sub { 'a0' },
                b => sub { 'b0' },
                c => sub { 'c0' },
                d => sub { 'd0' },
                e => sub { 'e0' },
            },
            export_versions => {
                v1 => {
                    export => [qw/a b c/],
                    export_anon => {
                        a => sub { 'a1' },
                        b => sub { 'b1' },
                        c => sub { 'c1' },
                    },
                },
                v2 => {
                    export => [qw/a b c/],
                    export_anon => {
                        a => sub { 'a2' },
                        b => sub { 'b2' },
                        c => sub { 'c2' },
                    },
                },
            },
        );
    }
}

{
    package My::Importer::A;
    use Importer 'My::Exporter' => qw/a b c d e/;
    ::is(a(), 'a0', "got versionless a()");
    ::is(b(), 'b0', "got versionless b()");
    ::is(c(), 'c0', "got versionless c()");
    ::is(d(), 'd0', "got versionless d()");
    ::is(e(), 'e0', "got versionless d()");
}

{
    package My::Importer::B;
    use Importer 'My::Exporter' => qw/d +v1 a b +v2 c +v0 e/;

    ::is(a(), 'a1', "got v1 a()");
    ::is(b(), 'b1', "got v1 b()");

    ::is(c(), 'c2', "got v2 c()");

    ::is(d(), 'd0', "got versionless d()");

    ::is(e(), 'e0', "got versionless e()");
}

{
    package My::Importer::C;
    use Importer 'My::Exporter' => ':v1';

    ::is(a(), 'a1', "got v1 a()");
    ::is(b(), 'b1', "got v1 b()");
    ::is(c(), 'c1', "got v1 c()");

    ::ok(!__PACKAGE__->can('d'), "Did not import d()");
}

done_testing;
