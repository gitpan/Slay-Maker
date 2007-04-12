package Slay::MakerRule ;

#
# Copyright (c) 1999 by Barrie Slaymaker, rbs@telerama.com
#
# You may distribute under the terms of either the GNU General Public
# License or the Artistic License, as specified in the README file.
#

=head1 NAME

Slay::MakerRule - a class for making things with dependancies

=head1 SYNOPSIS

   use strict ;

   use Slay::MakerRule ;

   $t1 = Slay::MakerRule->new(
      \@target,         ## Filenames made by \@actions
      \@dependencies,   ## Files or Slay::MakerRule objects
      \@actions,        ## Command lines or sub{}
   ) ;

Any or all of the three parameters may be scalars if there is only one
thing to pass:

   $t1 = Slay::MakerRule->new(
      $target,
      $dependency,
      $action,
   ) ;

=head1 DESCRIPTION

=over

=cut

use strict ;

use Carp ;
use Fcntl qw( :DEFAULT :flock ) ;
use File::Basename ;
use File::Path ;
use IPC::Run qw( run ) ;

use fields qw(
    ACTS
    CMD
    COMPILED_PATS
    DEPS
    OPTS
    PATS
    
    _IN_MAKE
) ;


sub new {
   my $proto = shift ;

   my Slay::MakerRule $self ;
   my $class = ref $proto || $proto ;
   {
      no strict 'refs' ;
      $self = bless [ \%{"$class\::FIELDS"} ], $class ;
   }

   $self->{PATS} = [] ;
   $self->{DEPS} = [] ;
   $self->{ACTS} = [] ;
   $self->{OPTS} = {} ;

   if ( ref $_[0] eq 'HASH' ) {
      ## It's a hash-style initter.
      my ( $h ) = @_ ;
      for ( keys %$h ) {
	 if ( /^(?:PATS|DEPS|ACTS)$/ ) {
	    $self->{$_} = $h->{$_} ;
	 }
	 else {
	    $self->{OPTS}->{$_} = $h->{$_} ;
	 }
      }

   }
   else {
      ## It's qw( patterns, ':', dependencies, '=', actions ).
      ## NB: The ':' and '=' may appear as the last char of a scalar param.
      $self->{OPTS} = pop if ref $_[-1] eq 'HASH' ;
      my $a = $self->{PATS} ;
      my $e ;
      my $na ;
      for ( @_ ) {
         $e = $_ ;
	 $na = undef ; ;
         unless ( ref $e ) {
	    if ( $e =~ /^:$/ )  { $a  = $self->{DEPS} ; next } 
	    if ( $e =~ /^=$/ )  { $a  = $self->{ACTS} ; next }
	    if ( $e =~ s/:$// ) { $na = $self->{DEPS} }
	    if ( $e =~ s/=$// ) { $na = $self->{ACTS} }
	 }
         push @$a, $e ;
	 $a = $na if defined $na ;
      }
   }
   
   return $self ;
}


=item check

Builds the queue of things to make if this target or it's dependencies are
out of date.

=cut

sub check {
   my Slay::MakerRule $self = shift ;
   my $user_options = ref $_[-1] ? pop : {} ;
   my ( $make, $target, $matches ) = @_ ;

   ## We join the options sets so that passed-in override new()ed, and
   ## we copy them in case somebody changes their mind.
   my $options = {
      %{$make->options},
      %{$self->{OPTS}},
      %$user_options,
   } ;

   print STDERR "$target: checking ".$self->targets." ", %$options, "\n"
      if $options->{debug} ;
   if ( $self->{_IN_MAKE} ) {
      warn "Ignoring recursive dependency on " . $self->targets ;
      return ;
   }

   my @required ;
   push @required, "forced" if $options->{force} ;
   push @required, "!exists" unless $make->e( $target ) ;

   if ( $options->{debug} && $make->e( $target ) ) {
      print STDERR (
	 "$target: size, atime, mtime: ",
	 join(
	    ', ',
	    $make->size( $target ),
	    scalar( localtime $make->atime( $target ) ),
	    scalar( localtime $make->mtime( $target ) ),
	 ),
	 "\n"
      ) ;;
   }

   ## If the queue grows when our dependencies are checked, then we must
   ## be remade as well.
   my $count = $make->queue_size ;

   my @deps = map {
      if ( ref $_ eq 'CODE' ) {
         $_->( $make, $target, $matches ) ;
      }
      elsif ( /\$/ ) {
         my $dep = $_ ;
	 ## TODO: Error out or provide a '' if $matches[n] undefined.
	 ## TODO: Make this in to 1 s/// so that double interpolation
	 ## won't occur if, say, $matches->[0] contains '${1}'
	 $dep =~ s/\$(\d+)/$matches->[$1-1]/g ;
	 $dep =~ s/\$\{(\d+)\}/$matches->[$1-1]/g ;
	 ## TODO: allow s///s from $ENV here
	 $dep =~ s/\$\{TARGET\}/$target/g ;
	 $dep ;
      }
      else {
         $_ ;
      }
   } @{$self->{DEPS}} ;

   print STDERR "$target: deps: ", join( ', ', @deps ), "\n"
      if $options->{debug} && @deps ;

   $make->check_targets( @deps, $user_options ) ;
   push @required, "!deps" if $make->queue_size > $count ;

   unless ( @required ) {
      ## The target exists && no deps need to be rebuilt.  See if the
      ## target is up to date.
      my $max_mtime ;
      for ( @deps ) {
	 print STDERR "$target: checking " . Cwd::cwd() . " $_\n"
	    if $options->{debug} ;
	 my $dep_mtime = $make->mtime( $_ ) ;
	 print STDERR "$target: $_ mtime " . localtime( $dep_mtime ) . "\n"
	    if $options->{debug} ;
	 $max_mtime = $dep_mtime
	    if defined $dep_mtime
	       && ( ! defined $max_mtime || $dep_mtime > $max_mtime ) ;
      }
      push @required, "out of date"
	 if defined $max_mtime && $max_mtime > $make->mtime( $target ) ;


   }

   if ( @required ) {
      print STDERR "$target: required ( ", join( ', ', @required ), " )\n"
	 if $options->{debug} ;
      $make->push( $target, $self, \@deps, $matches, $options ) ;
   }
   else {
      print STDERR "$target: not required\n"
	 if $options->{debug} ;
   }
}


sub _compile_pattern {
   my ( $pat ) = @_ ;

   my $exactness = -1 ;
   my $lparens    = 0 ;
   my $re ;
   if ( ref $pat ne 'Regexp' ) {
      $re = $pat ;
      ## '\a' => 'a'
      ## '\*' => '\*'
      ## '**' => '.*'
      ## '*'  => '[^/]*'
      ## '?'  => '.'
      $re =~ s{
	 (  \\.
	 |  \*\*
	 |  .
	 )
	 }{
	    if ( $1 eq '?' ) {
	       --$exactness ;
	       '[^/]' ;
	    }
	    elsif ( $1 eq '*' ) {
	       --$exactness ;
	       '[^/]*' ;
	    }
	    elsif ( $1 eq '**' ) {
	       --$exactness ;
	       '.*' ;
	    }
	    elsif ( $1 eq '(' ) {
	       ++$lparens ;
	       '(' ;
	    }
	    elsif ( $1 eq ')' ) {
	       ')' ;
	    }
	    elsif ( length $1 > 1 ) {
	       quotemeta(substr( $1, 1 ) );
	    }
	    else {
	       quotemeta( $1 ) ;
	    }
	 }xeg ;
      $re = "^$re\$" ;
   }
   else {
      ## Destroy it in order to get metrics.
      $re = "$pat" ;
      $re =~ s{
	 (
	    \\.
	    |\(\??
	    |(?:
	       .[?+*]+
	       |\.[?+*]*
	    )+
	 )
	 }{
	    if ( substr( $1, 0, 1 ) eq '\\' ) {
#		  print STDERR "\\:$1\n" if $options->{DEBUG} ;
	    }
	    elsif ( substr( $1, 0, 1 ) eq '(' ) {
#		  print STDERR "(:$1\n" if $options->{debug} ;
	       ++$lparens
		  if substr( $1, 0, 2 ) ne '(?' ;
	    }
	    else {
#		  print STDERR "*:$1\n" if $options->{debug} ;
	       --$exactness ;
	    }
	    ## Return the original value, just for speed's sake
	    $1 ;
	 }xeg ;
      ## Ok, now copy it for real
      $re = $pat ;
   }

#   print STDERR (
#      "re: $re\n",
#      "lparens: $lparens\n",
#      "exactness: $exactness\n",
#   ) if $options->{debug} ;

   return [ $re, $exactness, $lparens ] ;
}


=item exec

Executes the action(s) associated with this rule.

=cut

sub exec {
   my Slay::MakerRule $self = shift ;
   my $options = ref $_[-1] eq 'HASH' ? pop : {} ;
   my ( $make, $target, $deps, $matches ) = @_ ;

   my @output ;
   print STDERR "$target: in exec() for ". $self->targets.", ", %$options, "\n"
      if $options->{debug} ; 

   my $target_backup ;

   if (  ( $options->{detect_no_size_change} || $options->{detect_no_diffs} )
      && ! -d $target
   ) {
      $target_backup = $make->backup(
	 $target,
	 {
	    stat_only => ! $options->{detect_no_diffs},
	    move      => $options->{can_move_target},
	    debug     => $options->{debug},
	 }
      ) ;
   }

   if ( $options->{auto_create_dirs} ) {
      ## Use dirname so that 'a/b/c/' only makes 'a/b', leaving it up to the
      ## make rule to mkdir c/.  fileparse would return 'a/b/c'.
      my ( $dir ) = dirname( $target ) ;
      if ( ! -d $dir ) {
	 mkpath( [ $dir ] ) ;
	 warn "Failed to create $dir" unless -d $dir ;
      }
   }

   for my $act ( @{$self->{ACTS}} ) {
      local %ENV = %ENV ;
      $ENV{TARGET} = $target ;
      delete $ENV{$act} for grep {/^(DEP|MATCH)\d+$/} keys %ENV ;
      $ENV{"DEP$_"}   = $deps->[$_]    for (0..$#$deps) ;
      $ENV{"MATCH$_"} = $matches->[$_] for (0..$#$matches) ;

      if ( ref $act eq 'CODE' ) {
	 print STDERR "$target: execing CODE\n"
	    if $options->{debug} ;
         my $out = $act->( $make, $target, $deps, $matches ) ;
	 $out = '' unless defined $out ;
	 push @output, $out ;
      }
      elsif ( ref $act eq 'ARRAY' ) {
	 print STDERR "$target: execing ", join( ' ', map {"'$_'"} @$act ), "\n"
	    if $options->{debug} ;
         ## It's a command line in list form, so don't exec the shell
	 my $out ;
	 run $act, \undef, \$out ;
	 push( @output, $out ) ;
      }
      elsif ( ! ref $act ) {
	 $_ = $act;	# N.B. Work on a copy...
 	 s/\$(\d+)/$matches->[$1-1]/g ;
 	 s/\$\{(\d+)\}/$matches->[$1-1]/g ;
	 print STDERR "$target: execing '$_' \n"
	    if $options->{debug} ;
         ## It's a command line in string form
	 my $out ;
	 run [ 'sh', '-c', $_ ], \undef, \$out ;
	 $_ =~ m{(\S*)} ;
	 my $cmd = $1 ;
	 push( @output, $out ) ;
      }
      else {
         confess "Invalid type for a Slay::MakerRule rule: " . ref $act ;
      }
   }

   $make->clear_stat( $target ) ;
   my @new_stats = $make->stat( $target ) ;

   if ( defined $target_backup ) {
      $make->remove_backup(
         $target_backup,
	 {
	    restore_if_unchanged => 1,
	    deps                 => $deps
	 }
      ) ;
   }

   return wantarray ? @output : join( '', @output ) ;
}


=item targets

returns either ( target1, target2, ... ) or "target1, target2, ..." depending
on context.

=cut

sub targets {
   my Slay::MakerRule $self = shift ;
   return wantarray ? @{$self->{PATS}} : join( ', ', @{$self->{PATS}} );
}


=item matches

Checks the target list to see if it matches the target passed in.

=cut

sub matches {
   my Slay::MakerRule $self = shift ;
   my $options = ref $_[-1] eq 'HASH' ? pop : {} ;

   my ( $target ) = @_ ;

   my $max_exactness ;
   my @matches ;

   if ( ! $self->{COMPILED_PATS} ) {
      $self->{COMPILED_PATS} = [
         map {
	    _compile_pattern $_
	 } grep {
	    ref $_ ne 'CODE'
	 } @{$self->{PATS}}
      ] ;
   }
#print STDERR join("\n",map { join(',', @$_ ) } @{$self->{COMPILED_PATS}} ), "\n" ;
   for ( @{$self->{COMPILED_PATS}} ) {
      my ( $re, $exactness, $lparens ) = @$_ ;
#print STDERR "$target: ?= $re\n" ;
      if ( $target =~ $re &&
	 ( ! defined $max_exactness || $exactness > $max_exactness )
      ) {
	 $max_exactness = $exactness ;
	 no strict 'refs' ;
	 @matches = map {
	    ${$_}
	 } (1..$lparens) ;
#	 print STDERR (
#	    "$target: matches: ",
#	    join( ',', map { defined $_ ? "'$_'" : '<undef>' } @matches),
#	    "\n"
#	 ) if $options->{debug} ;

      }
   }

   return defined $max_exactness ? ( $max_exactness, \@matches ) : () ;
}

=back

=cut

1 ;
