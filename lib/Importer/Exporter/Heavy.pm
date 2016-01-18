package Importer::Exporter::Heavy;
use warnings;

use Importer::Exporter();

*{"heavy_$_"} = Importer::Exporter->can($_) for qw{
    export_fail
    export
    export_to_level
    require_version
    export_tags
    export_ok_tags
};


1;

__END__

=pod

=encoding UTF-8

=head1 NAME

Importer::Exporter::Heavy - DO NOT USE THIS

=head1 DESCRIPTION

This is what L<Exporter::Heavy> would look like if it used L<Importer> to get
the job done.

=head1 *** EXPERIMENTAL ***

This module is still experimental. Anything can change at any time. Testing is
currently VERY insufficient.

=head1 SOURCE

The source code repository for symbol can be found at
F<http://github.com/exodist/Importer>.

=head1 MAINTAINERS

=over 4

=item Chad Granum E<lt>exodist@cpan.orgE<gt>

=back

=head1 AUTHORS

=over 4

=item Chad Granum E<lt>exodist@cpan.orgE<gt>

=back

=head1 COPYRIGHT

Copyright 2015 Chad Granum E<lt>exodist7@gmail.comE<gt>.

This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

See F<http://dev.perl.org/licenses/>

=cut
