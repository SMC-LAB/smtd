#!/usr/bin/env perl

use strict; use warnings;
use Data::Dumper; use Getopt::Long qw/:config auto_version auto_help/;
use version; our $VERSION = '0.0.1';
use List::Util; use Carp; use Pod::Usage;

eval "require MIDI"
or die 'mtd.pl: requires the MIDI module to work. See <http://www.cpan.org/modules/INSTALL.html>.';

# prepare command-line options and arguments,
# check required options, add some boiler plate options (help, manual, etc)
# redirect input/output/err files to canonical handles,
# allow user to specify either positional or named patterns
# note that named patterns, if present, override positional ones
GetOptions(
    \%ARGV,
    'input|i=s', 'output|o=s', 'err|e=s',
    'note|n=i', 'magnitude|m=f',
    'pattern|p=f@{,}', 'named-pattern|np=f%',
    _meta_options(\%ARGV),
) and @ARGV{qw/note magnitude pattern/}
or pod2usage(-verbose => 1);

my ( $INH, $OUTH, $ERRH ) = _prepare_io( \%ARGV, \@ARGV );

$ARGV{'pattern'}
= named_to_positional_pattern($ARGV{'named-pattern'})
if $ARGV{'named-pattern'};


# read in midi file either from STDIN or file,
# depending on what user has specified.
# we need to check that the file format is v1,
# since I don't really understand all the possible variations.
my $opus = MIDI::Opus->new({
    @ARGV
    ? (from_file   => shift @ARGV)
    : (from_handle => $INH)
});

die 'mtd.pl: wrong MIDI file format: ',
    $opus->format,
    ' (should be format 1 --- synchronous multitracks)'
unless $opus->format == 1;


# main program:
# The strategy is to make all midi events' delta-times absolute,
# that is, relative to the first one.
my $abs_events = $opus->rel2abs;

# Then, change each event that falls within the grid
# *SORT* the events by their new absolute position
# since some later ones may now have a smaller value than earlier ones
my @abs_events
= sort { $a->[1] <=> $b->[1] }
map {
    $opus->mtd( $_, $ARGV{note}, $ARGV{magnitude}, $ARGV{pattern} )
}
@$abs_events;

# we can now replace the original events by the modified ones,
# after converting them back to relative offsets, knowing that
# there can be no negative offsets, due to the sorting.
$opus->tracks_r->[1]->events_r(
    $opus->abs2rel(\@abs_events)
);


# and we're done
# if we print to STDOUT, binary data might screw up the terminal
# $ reset if so.
$ARGV{output}
? $opus->write_to_file($ARGV{output})
: $opus->write_to_handle($OUTH);





# boiler-plate stuff
sub _meta_options {
    my ($opt) = @_;

    return (
        'quiet'     => sub { $opt->{quiet}   = 1;          $opt->{verbose} = 0 },
        'verbose:i' => sub { $opt->{verbose} = $_[1] // 1; $opt->{quiet}   = 0 },
        'version'   => sub { pod2usage( -sections => ['VERSION', 'REVISION'],
                                        -verbose  => 99 )                      },
        'license'   => sub { pod2usage( -sections => ['AUTHOR', 'COPYRIGHT'],
                                        -verbose  => 99 )                      },
        'usage'     => sub { pod2usage( -sections => ['SYNOPSIS'],
                                        -verbose  => 99 )                      },
        'help'      => sub { pod2usage( -verbose  => 1  )                      },
        'manual'    => sub { pod2usage( -verbose  => 2  )                      },
    );
}
sub _prepare_io {
    my ($opt, $argv) = @_;

    my ($INH, $OUTH, $ERRH);
    
    # If user explicitly sets -i, put the argument in @$argv
    unshift @$argv, $opt->{input} if exists $opt->{input};

    # Allow in-situ arguments (equal input and output filenames)
    if (    exists $opt->{input} and exists $opt->{output}
               and $opt->{input} eq $opt->{output} ) {
        open $INH, q{<}, $opt->{input}
            or croak "Can't read $opt->{input}: $!";
        unlink $opt->{output};
    }
    else { $INH = *STDIN }

    # Redirect STDOUT to a file if so specified
    if ( exists $opt->{output} and q{-} ne $opt->{output} ) {
        open $OUTH, q{>}, $opt->{output}
            or croak "Can't write $opt->{output}: $!";
    }
    else { $OUTH = *STDOUT }

    # Log STDERR if so specified
    if ( exists $opt->{error} and q{-} ne $opt->{error} ) {
        open $ERRH, q{>}, $opt->{error}
            or croak "Can't write $opt->{error}: $!";
    }
    elsif ( exists $opt->{quiet} and $opt->{quiet} ) {
        use File::Spec;
        open $ERRH, q{>}, File::Spec->devnull
            or croak "Can't write $opt->{error}: $!";
    }
    else { $ERRH = *STDERR }

    return ( $INH, $OUTH, *STDERR = $ERRH );
}

# utilities
sub named_to_positional_pattern {
    my ($named) = @_;

    my @positional = map { $named->{$_} || 0 }
    (1 .. List::Util::max(keys %$named));

    return \@positional;
}
sub group_by {
    my ($n, @list, @group_by) = @_;

    push @group_by, [splice @list, 0, $n]
    while @list;

    return @group_by;
}


# Monkey-patch MIDI::Opus for the syntactic sugar.
# this defines useful conversion functions, predicates, etc.
package MIDI::Opus;
sub beats_per_measure {
    return (
        List::Util::first { $_->[0] eq 'time_signature' } 
        shift->tracks_r->[0]->events
    )->[2];
}
sub set_tempo {
    return (
        List::Util::first { $_->[0] eq 'set_tempo' } 
        shift->tracks_r->[0]->events
    )->[2];
}
sub bpm { return 60_000_000 / $_[0]->set_tempo }
sub mu_per_tick { return $_[0]->set_tempo / $_[0]->ticks }
sub ms_per_tick { return $_[0]->mu_per_tick / 1000 }
sub ms_to_ticks { return sprintf("%.0f", $_[1] / $_[0]->ms_per_tick) }
sub mtd {
    my ($self, $event, $note, $magnitude, $weights) = @_;

    if (my $weight_index = $self->in_grid($event, $note, 0)) {

        $event->[1]
        += sprintf(
            "%.0f",
            $self->ms_to_ticks($magnitude * $weights->[$weight_index])
        );
    }

    return $event;
}
sub tick_length {
    return List::Util::sum
    map { $_->[1] }
    $_[0]->tracks_r->[1]->events
}
sub note_count { 
    return sprintf(
        "%.0f",
        ($_[0]->tick_length / $_[0]->ticks)
        * ($_[1] / $opus->beats_per_measure)
    )
}
sub get_grid {
    my ($self, $note) = @_;

    return
    main::group_by(
        $note,
        map { $_ * $self->ticks / ($note / $opus->beats_per_measure) }
        0 .. $self->note_count($note)
    );;
}
{
    my $tick = 0;

    sub in_grid {
        my ($self, $event, $note, $tolerance) = @_;

        $tick += $event->[1];
        $tolerance = $self->ms_to_ticks($tolerance);

        for my $measure ($self->get_grid($note)) {
            for my $nth_note (1 .. @$measure) {
                return $nth_note - 1
                if  $tick >= $measure->[$nth_note - 1] - $tolerance
                and $tick <= $measure->[$nth_note - 1] + $tolerance;
            }                
        }
        return;
    }
}
sub scale_grid {
    my ($self, $factor) = @_;

    $self->ticks( $self->ticks * $factor );

    for ( @{$self->tracks_r->[1]->events_r} ) {
        $_->[1] *= $factor;
    }
}
sub shift_grid {
    my ($self) = @_;

    my $events = $self->tracks_r->[1]->events_r;

    for ( 1 .. @$events - 1 ) {

        if ($events->[$_] < 0) {
            $events->[$_ - 1] -= $events->[$_];
            $events->[$_] = 0;
        }
    }
}
sub rel2abs {
    my ($self) = @_;

    my @abs = ($self->tracks_r->[1]->events_r->[0]);

    for (@{$self->tracks_r->[1]->events_r}) {
        my $accumulator = $abs[-1]->[1] + $_->[1];
        push @abs, [$_->[0], $accumulator, @{$_}[2 .. @{$_}-1]];
    }

    return \@abs;
}
sub abs2rel {
    my ($self, $abs) = @_;

    my @rel = ($abs->[0]);

    push @rel, [
        $abs->[$_][0], 
        $abs->[$_][1] - $abs->[$_ - 1][1],
        @{$abs->[$_]}[2 .. @{$abs->[$_]} - 1]
    ]
    for 1 .. @$abs - 1;
    
    return \@rel;
}

__END__


=head1 NAME

 mtd.pl - Introduce systematic microtiming deviations into a MIDI file


=head1 SYNOPSIS

 mtd.pl -n NOTE_VALUE -m MAGNITUDE -p WEIGHT_i[, WEIGHT_n[,...]] < rhythm.mid > mtd-rhythm.mid


=head1 DESCRIPTION

 Takes a format-1 MIDI file via STDIN, positional argument or named argument (-i, --input),
 and prints it to STDOUT or a file (-o, --output) with certain note_on and note_off events
 offset by a weighted magnitude, given in milliseconds and coefficients, respectively.
 

=head1 OPTIONS

 -n,  --note          <integer>         note durations in pattern (1 - whole note, 2 - half note, 4 - quarter note, ...)
 -m,  --magnitude     <decimal>         maximum deviation in milliseconds
 -p,  --pattern       <decimals ...>    weights to apply to each note position in measure (list size must be equal to --note argument)
 -np, --named-pattern [-np k=v -np ...] values to apply as coefficients to each respective key position
 -o,  --output        [string]          output filename                          (STDOUT)
      --verbose       [integer]         print increasingly verbose error messages
      --quiet                           print no diagnostic or warning messages
      --version                         print current version
      --license                         print author's contact and copyright information
      --help                            print this information
      --manual                          print the plain old documentation page


=head1 VERSION

 0.0.1


=head1 AUTHOR

 Pedro Silva <pasilva@inescporto.pt>
 Sound and Music Computing Group
 Telecommunications and Multimedia Group
 INESC Porto


=head1 COPYRIGHT

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program. If not, see <http://www.gnu.org/licenses/>.

=cut
