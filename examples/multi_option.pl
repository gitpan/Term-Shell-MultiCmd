
use strict ;
use warnings ;
use lib 'lib', '../lib' ;
use Term::Shell::MultiCmd;
my @command_tree =
  ( 'confess' =>
    { help => 'Just testing multi options
try:
confess -name="Some String" -flag 2 -flag 3 -flag 4 -force
',
      opts => 'force name=s flag=i@',
      exec => \&confession
    }
  ) ;

sub confession {
    my ($o, %p) = @_ ;
    print "Running this command, the value of \%p is:\n" ;
    use Data::Dumper ;
    print Dumper \%p ;
}

Term::Shell::MultiCmd
  -> new()
  -> populate( @command_tree )
  -> loop ;

