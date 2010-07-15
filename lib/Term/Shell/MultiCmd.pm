
package Term::Shell::MultiCmd;

use warnings;
use strict;
use Carp ;

=head1 NAME

Term::Shell::MultiCmd - Shell Interface with nested commands tree

=head1 VERSION

Version 1.02

=cut

our $VERSION = '1.02';

=head1 SYNOPSIS

    # More examples available with the distribution, under directory 'examples/'

    use Term::Shell::MultiCmd;
    my @command_tree =
     ( 'multi word command' =>
             { help => "Help title.",
               opts => 'force repeat=i',
               exec => sub {
                   my ($o, %p) = @_ ;
                   print "$p{ARG0} was called with force=$p{force} and repeat=$p{repeat}\n"
               },
             },
       'multi word another command' =>
             { help => 'Another help title.
  Help my have multi lines, the top one
  would be used when one linear needed.',
               comp => sub {
                   # this function would be called when use hits tab completion at arguments
                   my ($o, $word, $line, $start, $op, $opts) = @_ ;
                   # .. do something, then
                   return qw/a list of completion words/ ;
               },
               exec => sub { my ($o, %p) = @_ ; print "$p{ARG0} was called\n"},
             },
       'multi word third command' =>
             { help => 'same idea',
               comp => [qw/a list of words/], # this is also possible
               exec => sub { my ($o, %p) = @_ ; print "$p{ARG0} was called. Isn't that fun?\n"},
             },
       'multi word' => 'You can add general help title to a path',
     ) ;

     Term::Shell::MultiCmd
      -> new()
      -> populate( @command_tree )
      -> loop ;

    print "All done, see you later\n" ;

=head1 NOTE

To get the most from a command line, it might be a good idea to get the latest versions of
Term::ReadLine and Term::ReadKey.
There are numberless ways of doing it, one of them is running 'cpan update Bundle::CPAN' (with a proper write permission).

=cut

# some of my common utility functions:
sub _params($@) {

    # convert parameter to hash table, at this point,
    # I wish perl would have followed python's function
    # parameters scheme, or made Params::Smart standard.
    # (Had anybody mentioned perl6?)

    # Note 1: this parameter processing takes time, and wouldn't
    # be a good choise for frequently called functions.

    # Note 2: as parameters are suplied by developer, a bad
    # would terminate the program - this is not a sandbox.

    my %ret ;
    my $str = shift ;
    for (split ' ', $str) {
        /(\w+)(\=(.*))?/ or confess "_params can only take simple instructions
like key (must be provided), or key=value (value becomes default), or key= (default empty string)
" ;
        $ret{$1} = $2 ? $3 : undef ;
    }
    # when called as OO, itemize self
    # Note: this one wouldn't work with classes (as in Term::Shell::MultiCmd -> new )
    $ret{self} = shift if $_[0] and ref $_[0] ;
    while (@_) {
        my ($k, $v) = (shift, shift) ;
        $k =~ s/^\-?\-?// unless ref $k ;
        croak "unknown parameter: '$k'\n expected $str\n" unless exists $ret{$k} ;
        $ret{$k} = $v ;
    } ;
    while (my ($k, $v) = each %ret) {
        croak "missing parameter: '$k'\n expected $str\n" unless defined $v ;
    }
    %ret
}

sub _options {
    # Parsing user's options, this function is more forgiving than _params
    my $p = shift ;
    my @p = ref $p ? @$p : split ' ', $p ;
    my %p ; # now we have a complete set

    # use Getopt::Long 'GetOptionsFromArray' ; -- didn't work as I expected ..
    use Getopt::Long ;
    local @ARGV = @_ ;
    unless ( eval { GetOptions( \%p, @p ) } ) {
        $p{_ERR_} = "$@ Expected " . join ', ', map {/(\w+)/ ; '-' . ($1 || $_)} sort @p ;
        $p{_ERR_} .= "\n" ;
    }
    $p{ARGV} ||= [@ARGV] if @ARGV ; # if caller had chosen ARGV as parameter, maybe he knows what he's doing
    %p
}

# we can't limit ourselves by 'use :5.10', not yet.
sub _say(@) { print join ('', @_) =~ /^\n*(.*?)\s*$/s, "\n" }


# module specific functions

# Note: manipulate $o->{delimiter} and $o->{delimiterRE} ONLY if you know what you're doing ...
sub _split($$) {
    my ($o, $l) = @_ ;
    use Text::ParseWords 'quotewords';
    grep {$_} quotewords $o->{delimiterRE} || '\s+', 0, $l
}

sub _join($@) {
    my $o = shift ;
    join $o->{delimiter} || ' ', @_
}

sub _travel($@) {
    my ($o, @w) = @_ ;
    my $c = $o->{cmds} ;
    my @path ;
    while( @w and 'HASH' eq ref $c ) {
        my $w = shift @w ;
        if (exists $c->{$w}) {
            $c = $c->{$w} ;
            push @path , $w ;# $path .= "$w ";
            next ;
        }
        my @c = grep /^$w/, keys %$c ;
        if(@c == 1) {
            $c = $c->{$c[0]} ;
            push @path, $c[0] ; # $path .= "$c[0] " ;
            next ;
        }
        if (@c > 1 ) {
            my $cmd = join $o->{delimiter} || ' ', @path, $w ;
            return "Ambiguous command: '$cmd'\n $w could mean: @c\n" ;
        }

        # if @c == 0 : should I state the obvious? well, not with perl
        unshift @w, $w ;
        last ;
    }
    ($c, join ($o->{delimiter} || ' ', @path), @w)
}

sub _expect_param_comp {
    my($o, $word, $line, $pos, $op, $opt) = @_;
    # This is ugly, Getopt::Long has many options, and
    # caller can use any of them. However, my parsing would
    # be limited.
    print "$opt\n" ;
    my ($t) = $opt =~ /(\=\w)\W*$/ ;
    # $t or return ('', $word) ;q
    my $type = {'=i' => 'Integer',
                '=o' => 'Extended Integer',
                '=s' => 'String',
                '=f' => 'Real Number',
               }->{$t} || $t ;
    ("Parameter Expected for -$op, type '$type'", $word)
}

sub _filter($@) {
    my $w = shift ;
    my $qr = qr/^\Q$w/ ;
    grep /$qr/, sort grep {$_ ne $;}
      'ARRAY'  eq ref $_[0] ? @{$_[0]} :
        'HASH' eq ref $_[0] ? (keys %{$_[0]}) :
          @_   ;
}

=head1 SUBROUTINES/METHODS

=head2 new

    my $cli = new Term::Shell::MultiCmd ;
   - or -
    my $cli = Term::Shell::MultiCmd->new( [optional parameters ...] ) ;

The parameters to the constructor are passed in hash form, preceding dash is optional.

Optional Parameters for the new command:

=over 4

=item * -prompt

my $cli = Term::Shell::MultiCmd ( -prompt => 'myprompt> ') ;

Set prompt to myprompt> (including the bigger than sign). Default is 'shell> '.

=item * -help_cmd

Overwrite the default 'help' command, empty string would skip adding this command.

=item * -quit_cmd

Overwrite the default 'quit' command, empty string would skip adding this command.

=item * -history_file

This is the history file name. If present, try to load history from this file at
the before the loop command, and try saving history in this file after the loop command.
Default is empty string (i.e. no history preserved between sessions).

=item * -history_size

If history_file exists, set the number of history items to preserve. Default is 100.

=item * -history_more

If history_file exists, try to load this data from the file before the loop, and try save it after.
For Example:
 my %user_defaults ;
 my $cli = new Term::Shell::MultiCmd ( -history_file => "$ENV{HOME}/.my_saved_data",
                                       -history_size => 200,
                                       -history_more => \%user_defaults,
                                     ) ;
 ....
 $cli -> loop ;

 This would load shell's history and %user_defaults from the file .my_saved_data before the loop, and
 store 200 history entries and %user_defaults in the file after the loop.

Please note:
 - The value of history_more must be a reference for HASH, ARRAY, or SCALAR
 - No warnings would be provided if any of the operations fail. Use it on your own risk.

=back

=cut

my $loop_stop ;
sub new {

    my $class = shift ;
    my %p = _params 'pager= help_cmd=help quit_cmd=quit prompt=shell>
                     history_file= history_size=100 history_more=
                     ', @_ ;
    # structure rules:
    # hash ref is a path, keys are items (commands or paths) special item $; is one liner help
    # array ref is command's data as [help, command, options, completion]
    #  where: first help line is the one liner, default completion might be good enough

    my $o = bless { cmds => { },
                    map {($_, $p{$_})} qw/prompt pager history_file history_size history_more/
                  }, ref ( $class ) || $class ;

    $o -> add_exec ( path => $p{help_cmd},
                     exec => \&_help_command,
                     comp => \&_help_command_comp,
                     opts => 'recursive tree',
                     help => 'help [command or prefix]
Options:
  -t --tree      : Show commands tree
  -r --recursive : Show full help instead of title, recursively
'                   ) ;
    $o -> add_exec ( path => $p{quit_cmd},
                     exec => sub {$_[0]->{stop} = 1 },
                     help => 'Exit this shell',
                   ) ;

    use Term::ReadLine;
    my $t = eval { local $SIG{__WARN__} = 'IGNORE' ;
                   $o -> {term} = Term::ReadLine->new($o->{prompt}) } ;
    die "Can't create Term::ReadLine: $@\n" if ! $t and -t select ;

    if (defined $readline::rl_completion_function) {
        $readline::rl_completion_function =
          sub { $o -> _complete_cli(@_)} ;
    }
    elsif ( defined (my $attr = $t -> Attribs())) {
        $attr->{attempted_completion_function} =
          sub { $o -> _complete_gnu(@_) } ;
    }
    else {
        warn __PACKAGE__ . ": no tab completion support for this system. Sorry.\n" ;
    }

    $o
}


=head2 add_exec

   $cli -> add_exec ( -path => 'full command path',
                      -exec => \&my_command,
                      -help => 'some help',
                      -opts => 'options',
                      -comp => \&my_completion_function,
                    ) ;

This is function adds an command item to the command tree. Its options are complected, but useful (or so I hope).

=over

=item * -path

Mandatory. Expecting a string.
This string would be parsed as multi-words command.

Note: by default, this module expects whitespaces delimiter. If you'll read the module's code, you can find
an easy way to change it - in unlikely case you'll find it useful.

=item * -exec

Mandatory. Expecting a function ref.
This code would be called when the user types a unique path for this command (with optional
options and arguments). Parameters sent to this code are:
    my ($cli, %p) = @_ ;
where:
   $cli     - self object.
   $p{ARG0} - the command's full path (user might have used partial, but unique path. but this is the full explicit one)
   $p{ARGV} - all user arguments, in order (ARRAY ref)
   %p       - contains other options (see 'opts' below)

=item * -help

Expecting a multi line string.
The top line would be presented when a one line title is needed (for example, when 'help -tree'
is called), the whole string would be presented as the full help for this item.

=item * -comp

Expecting CODE, or ARRAY ref.
If Array, when user hits tab completion for this command, try to complete his input with words
from this list.
If Code, call this function with the next parameters:
    my ($cli, $word, $line, $start) = @_ ;
    # where:
    # $cli is the Term::Shell::MultiCmd object.
    # $word is the curent word
    # $line is the whole line
    # $start is the current location

This code should return a list of strings. Term::ReadLine would display those words (unless a single one)
and complete user's line to the longest common part. In other words - it would do what you expect.

For more information, see Term::ReadLine's man page.

=item * -opts

Expecting a string, or ARRAY ref.
If a string, split it to words by whitespaces. Those words are parsed as
standard Getopt::Long options. For example:
     -opts => 'force name=s flag=i@'

Would populating the previously described %p hash, correspond to user command:
     shell> user command -name="Some String" -flag 2 -flag 3 -flag 4 -force

As ARRAY ref, caller can also add a complete 'instruction' after each non-flag option (i.e. an option that
expects parameters). Like the 'comp' above, this 'instruction' must be an ARRAY or CODE ref, and follow
the same roles. If omitted, a default function would be called and ask user for input. For example:
    -opts => [ 'verbose' =>
               'file=s'  => \&my_filename_completion,
               'level=i' => [qw/1 2 3 4/],
             ],

For more information, see Getopt::Long's man page. Also see examples/multi_option.pl in distribution.

=back

=cut

sub add_exec {
    my $o = shift ;
    my %p = _params 'path exec help= comp= opts=', @_ ;
    return unless $p{path};     # let user's empty string prevent this command
    my $r = $o ->{cmds} ;
    my $p = '' ;
    die "command must be CODE refferance\n" unless 'CODE' eq ref $p{exec} ;
    my @w = _split $o, $p{path} ;
    my $new = pop @w or return ;
    for my $w (@w) {
        $p .= "$w " ;
        if ('ARRAY' eq ref $r ->{$w} ) {
            carp "Overwrite command '$p'\n" ;
            delete $r -> {$w} ;
        }
        $r = ($r->{$w} ||= {}) ;
    }
    my ($opts, %opts) = '' ;    # now calculate options
    if ($p{opts}) {
        my @opts = ref $p{opts} ? @{$p{opts}} : split ' ', $p{opts} ;
        # croak "options -opts must be ARRAY ref\n" unless 'ARRAY' eq ref $p{opts} ;
        while (@opts) {
            my $op = shift @opts ;
            croak "unexpected option completion\n" if ref $op ;
            $opts .= "$op " ;
            my $expecting = $op =~ s/\=.*$// ;
            $opts{$op} = ( $expecting  ?
                           ref $opts[0] ?
                           shift @opts :
                           \&_expect_param_comp :
                           '' ) ;
        }
    }
    #                   0    1    2       3      4..
    $r->{$new} = [@p{qw/help exec comp/}, $opts, %opts]
}


=head2 add_help

Although caller can set help via the add_exec, this command is useful when he wishes to
add title (or hint) to a part of the command path. For example:
   # assume $cli with commands 'feature set', 'feature get', etc.
   $cli -> add_help ( -path => 'feature' ,
                      -help => 'This feature is a secret') ;

=cut

sub add_help {
    my $o = shift ;
    my %p = _params "path help", @_ ;
    my ($cmd, $path, @args, $ret) = _travel $o, _split $o, $p{path} ;
    if ('HASH' eq ref $cmd) {
        for my $w (@args) {
            $cmd = ($cmd->{$w} = {});
        }
        ($ret, $cmd->{$;}) = ($cmd->{$;}, $p{help})
    }
    else {
        croak "command '$p{path}' does not exists.\n For sanity reasons, will not add help to non-existing commands\n" if @args;
        ($ret, $cmd->[0 ]) = ($cmd->[0 ], $p{help})
    }
    $ret # Was it worth the trouble?
}

=head2 populate

A is a convenient way to define a chain of add_exec and add_help commands. This function expects hash, where
the key is the command path and the value might be HASH ref (calling add_exec), or a string (calling add_help).
For example:
    $cli -> populate
       ( 'feature' => 'This feature is a secret',
         'feature set' => { help => 'help for feature set',
                            exec => \&my_feature_set,
                            opts => 'level=i',
                            comp => \&my_feature_set_completion_function,
                          },
         'feature get' => { help => 'help for feature get',
                            exec => \&my_feature_get
                          },
       ) ;

Note: Since the key is the path, '-path' can (and should) be omitted from add_exec parameters.

=cut

sub populate {
    my ($o, %p) = @_ ;
    while (my ($k, $v) = each %p) {
        if (not ref $v) {
            $o->add_help(-path => $k, -help => $v) ;
        }
        elsif ('HASH' eq ref $v) {
            $o->add_exec(-path => $k, %$v)
        }
        else {
            croak "unknow item for '$k': $v\n" ;
        }
    }
    $o
}

=head2 loop

  $cli -> loop ;

Prompt, parse, and invoke in endless loop

('endless' shouldn't be taken literally. Eventually user quits, or system crashes, or universe collapses - They always do.)

=cut

sub _last_setting_load($) {
    my $o = shift ;
    my $f = $o->{history_file} or return ;
    my $d = $o->{history_more} ;
    eval {
        open F, $f or return ;
        local $/ ;
        our $VAR1 ;
        $VAR1 = eval <F> ;
        my ($hist, $more) = @$VAR1 ;
        close F ;
        $o -> history ( @$hist ) if 'ARRAY' eq ref $hist ;
        return unless ref $d and ref $more and ref($d) eq ref($more) ;
        %$d = %$more if 'HASH'   eq ref $d ;
        @$d = @$more if 'ARRAY'  eq ref $d ;
        $$d = $$more if 'SCALAR' eq ref $d ;
    } ;
}

sub _last_setting_save($) {
    my $o = shift ;
    my $f = $o->{history_file} or return ;
    eval {
        use Data::Dumper ;
        my @his = $o -> history();
        splice @his, 0,  @his - $o->{history_size} ;
        open  F, '>', $f or return ;
        print F Dumper [[@his], $o->{history_more}] ; # Note: For backward compatibly, this array can only grow
        close F ;               # why bother closing? darn habits
        # print ":Status saved: $f\n" ;
    }
}

sub loop {
    local $| = 1 ;
    my $o = shift ;
    _last_setting_load $o ;
    while ( not $o -> {stop} and
            defined (my $line = $o->{term}->readline($o->{prompt})) ) {
        $o->cmd( $line ) ;
    }
    _last_setting_save $o ;
}

sub _complete_gnu {
    # my($o, $text, $line, $start, $end) = @_;
   &_complete_cli                # apparently, this should work
}

sub _complete_cli {
    my($o, $word, $line, $start) = @_;
    #   1. complete command
    #   2. if current word starts with '-', complete option
    #   3. if previous word starts with '-', try arg completion
    #   4. try cmd completion (should it overwirte 3 for default _expect_param_comp?)
    #   5. show help, keep the line

    my @w = _split $o ,        # should I ignore the rest of the line?
      substr $line, 0, $start ; # well, Term::ReadLine expects words list.

    my ($cmd, $path, @args) = _travel $o, @w ;
    return ($cmd, $word) unless ref $cmd ;
    return _filter $word, $cmd if 'HASH' eq ref $cmd ;

    my ($help, $exec, $comp, $opts, %opts) = @{ $cmd } ; # avoid confusion
    return map {"$1$_"} _filter $2,\%opts if $word =~ /^(\-\-?)(.*)/ ;
    if ( $w[-1] =~ /^\-\-?(.*)/ ) {
        my ($op, @op) = _filter $1, \%opts ;
        return ("Option $w[-1] is ambiguous: $op @op?", $word) if @op ;
        return ("Option $w[-1] is unknown", $word) unless $op ;
        my $cb = $opts{$op} ;
        return _filter $word, $cb if 'ARRAY' eq ref $cb ;
        return $cb->($o, $word, $line, $start, $op, $opts =~ /$op(\S*)/ ) if 'CODE' eq ref $cb ;
    }
    return _filter $word, $comp if 'ARRAY' eq ref $comp ;
    return $comp->($o, $word, $line, $start) if 'CODE' eq ref $comp ;
    return ($help, $word)       # so be it
}


sub _help_message_tree {        # inspired by Unix 'tree' command
                                # Should I add ANSI colors?
    my ($h, $cmd, $pre, $last) = @_ ;
    print $pre . ($last ? '`' : '|') if $pre ;
    return _say "- $cmd : ", $h->[0] =~ /^(.*)/m if 'ARRAY' eq ref $h ;
    _say "-- $cmd" ;
    my @c = sort keys %$h ;
    for my $c (@c) {
        _help_message_tree( $h->{$c},
                            $c,
                            $pre ? $pre . ($last ? '    ' : '|   ') : ' ' ,
                            $c eq ($c[-1]||'')
                          ) unless $c eq $; ;
    }
}

sub _help_message {
    my $o = shift ;
    my %p = _params "node path full= recursive= tree= ARGV= ARG0=", @_ ;
    my ($h, $p) = @p{'node', 'path'} ;
    $p =~ s/^\s*(.*?)\s*$/$1/ ;

    if ('ARRAY' eq ref $h) {    # simple command, full help
        _say "$p:\n $h->[0]" ;
        $h->[0]
    }
    elsif ('HASH' ne ref $h) {  # this one shouldn't happen
        confess "bad item in help message: $h"
    }
    elsif ($p{recursive}) {     # show everything

        _say "$p:\t", $h->{$;} if exists $h->{$;} ;

        for my $k (sort keys %$h) {
            next if $k eq $; ;
            _help_message( $o, %p, -node => $h->{$k}, -path => _join $o, $p, $k) ;
        }
    }
    elsif ($p{tree}) {          # tree - one linear for each one
        _help_message_tree ($h, $p)
    }
    elsif ($p{full}) {          # prefix, full list

        _say "$p:\t", $h->{$;} if exists $h->{$;} ;

        for my $k (sort keys %$h) {
            next if $k eq $; ;
            _say _join($o, $p, $k), ": \t",
              (('ARRAY' eq ref $h->{$k}) ?
               ($h->{$k}[0]  || 'a command' ) :
               ($h->{$k}{$;} || 'a prefix' ) ) =~ /^(.*)$/m ;
        }
    }
    else {                      # just show the prefix with optional help
        _say "$p: \t", $h->{$;} || 'A command prefix' ;
    }
}

sub _help_command {
    my ($o, %p) = @_ ;
    my ($cmd, $path, @args) = _travel $o, @{$p{ARGV}} ;
    return _say $cmd unless ref $cmd ;
    return _say "No such command or prefix: $path @args" if @args ;
    return _help_message($o, -node => $cmd, -path => $path, -full => 1, %p) ;
}

sub _help_command_comp {
    my($o, $word, $line, $start) = @_;
    my @w = _split $o , substr $line, 0, $start ;
    shift @w ;
    my ($cmd, $path, @args) = _travel $o, grep {!/\-\-?r(?:ecursive)?|\-\-?t(?:ree)?/} @w ;
                             # potential issue: 'help -r some path' wouldn't be a valid path, is DWIM the solution?
    return ($cmd, $word) unless ref $cmd ;
    return _filter $word, $cmd if 'HASH' eq ref $cmd ;
    ('', $word)
}


=head2 cmd

 $cli -> cmd ( "help -tree" ) ;

Execute the given string parameter, similarly to user input.

=cut

sub cmd {
    my $o = shift ;
    my @w = _split $o, shift or return ;
    my ($cmd, $path, @args) = _travel $o, @w ;
    return print $cmd unless ref $cmd ;
    return _help_message($o, -node => $cmd, -path => $path) unless 'ARRAY' eq ref $cmd ; # help message
    my %p = _options $cmd->[3] || '', @args ;
    return print $p{_ERR_} if $p{_ERR_} ;
    my ($fh, $fn, $stdout, $pg) ;

    # if ($pg = $o->{pager} and -x $pg ) {
    #     use File::Temp 'tempfile' ;
    #     ($fh, $fn) = tempfile() ;
    #     $o->{stdout} = select $fh ;
    # }

    my $res = $cmd->[1]->($o, ARG0 => $path, %p) ;

    # if ($fh) {
    #     $fh -> close () ;
    #     # Todo: Pager Support
    #     # use Term::ReadKey ;
    #     # my $hight = Term::ReadKey::GetTerminalSize($stdout) ;

    #     system "$pg $fn" ;
    # }
    # print "\n"

    $res
}

=head2 history

set/get history

  my @hist = $cli -> history() ;            # get history
  $cli -> history( @alternative_history ) ; # set history
  $cli -> history([@alternative_history]) ; # the very same, by ptr
  $cli -> history([]) ;                     # clear history

=cut

sub history {
    my $o = shift ;
    return $o->{term}->SetHistory(map {('ARRAY' eq ref $_) ? (@$_) : ($_)} @_ ) if @_ ;
    return $o->{term}->GetHistory
}


# =head2 pager

#     my $old_pager = $o->pager($new_pager);  # set new pager
#     my $old_pager = $o->pager('') ;         # clear pager
#     my $cur_pager = $o->pager() ;           # keep current pager

# =cut

# sub pager {
#     my ($o, $new) = @_ ;
#     my $old = $o->{pager} ;
#     $o->{pager} = $new if defined $new ;
#     $old
# }

=head1 ALSO SEE

Term::ReadLine, Term::ReadKey, Getopt::Long

=head1 AUTHOR

Josef Ezra, C<< <jezra at sign cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to me, or to C<bug-term-cli at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Term-CLI>.
I am grateful for your feedback.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Term::Shell::MultiCmd

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Term-CLI>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Term-CLI>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Term-CLI>

=item * Search CPAN

L<http://search.cpan.org/dist/Term-CLI/>

=back


=head1 ACKNOWLEDGMENTS

This module was inspired by the excellent modules Term::Shell,CPAN, and
CPANPLUS::Shell.

=head1 LICENSE AND COPYRIGHT

Copyright 2010 Josef Ezra.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

'end'

