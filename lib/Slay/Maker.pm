package Slay::Maker ;

$VERSION=0.02 ;

=head1 NAME

Slay::Maker - An perl make engine using perl code for rules

=head1 STATUS

Beta. Pretty stable, though underdocumented.

=head1 DESCRIPTION

Slay::Maker is a make engine that uses perl declaration syntax for
rules, including regular expressions for targets and anonymous subs
for targets, dependancies, and actions.

This allows you to tightly integrate a make engine in an application
and to excercise a large amount of control over the make process, taking
full advantage of Perl code at any point in the make cycle.

=head1 RULE SYNTAX

The rulebase syntax is:

   [ @targets1, ':', @dependancies1, '=', @actions1, { option => val } ],
   [ @targets2, ':', @dependancies2, '=', @actions2, { option => val } ],
   [ @targets3, ':', @dependancies3, '=', @actions3, { option => val } ],
   [ @targets4, ':', @dependancies4, '=', @actions4, { option => val } ],
   ...

Each item in any of the three arrays may be a literal string or a 
subroutine (CODE) reference.  A literal string is pretty much the same as
using a literal string in a regular makefile.  You may also use regular
expression ('Regexp') references (C<qr/.../>) in @targets and the
C<$1>, C<$2>, ... variables inside strings in @dependancies:

   [ qr/(.*).pm/, ':', "$1.pm.in", '=', sub { ... } ],

Subroutine references are evaluated as lazily as possible when the
make is being run, so any CODE refs in @targets will be called
each time a make is run, CODE refs in @dependancies will only be
called if a target matches, and CODE refs in @actions are only
called if the rule is fired.

=head2 TARGET SUBS

** NOT IMPLEMENTED QUITE YET **.  It's simple to do, just haven't needed
it yet.

Aside from strings and Regexps, you will be able to use CODE refs in
the target list.  These are called each time the rule is evaluated,
which will usually happen once per target or dependancy being 
checked when the make is run.

A target sub declaration might look like:

   sub {
      my ( $maker ) = @_ ;
      ...
      return @targets;
   },

(if target subs were implemented already).

=head2 DEPENDANCIES

Dependancies may be strings or CODE references.  Plain strings have
$1, $2, ... interpolation done on them (remember to \ escape the $1, etc.).

CODE refs will be called if the target matches and must return a 
possibly empty list of strings containing the names of dependancies.
Variable interpolation will not be done on the returned strings.  That
would be obscene.

A dependancy sub declaration might look like:

   sub {
      my ( $maker, $target, $matches ) = @_ ;
      ...
      return @dependancies ;
   },

where

   $maker    refers to the Slay::Maker (or subclass) being run
   $target   is the target that matched (in the event of multiple targets)
   $matches  is an ARRAY of the values extracted from $1, $2, etc.

.

=head2 ACTIONS

If an $action is a plain string, it's passed to "sh -c '$string'".  If
it's an ARRAY ref, it's run without interference from or benefit of
a shell (see L<IPC::Run/run> for details).  If it's a CODE ref, it's
called.

An action sub declaration might look like:

   sub {
      my ( $maker, $target, $deps, $matches ) = @_ ;
      ...
      return @dependancies ;
   },

where

   $maker    refers to the Slay::Maker (or subclass) being run
   $target   is the target that matched (in the event of multiple targets)
   $deps     is an ARRAY of the expanded dependancies.  There's no way
             of telling which are freshly rebuilt, but you can track that
	     yourself in the action rules of the dependancies, if you
	     like.
   $matches  is an ARRAY of the values extracted from $1, $2, etc.

=head1 TARGET BACKUPS

A target may be moved off to a backup location before it is rebuilt, so
that it may be restored if rebuilding fails.  This is also used for
the optional restoral of modification times described below.

Restoral needs to be done manually by calling the L</restore> method,
and you can call the L</backup> method, too.

The L</backup> method will be called automatically if modification
time restoral is enabled for a target.

=head1 MODIFICATION TIME RESTORAL

One unusual option is that a target file's modification time can
be restored if it is unchanged after being updated.  This can be
useful when checking files out of a repository: the files' mod times
won't be affected if their contents haven't changed.

This can be done by a (fast but possibly misleading) check for a change
in file size or by executing 'diff --brief' between a target's backup
and it's new version.  Other methods, such as hashing or block-by-block
binary comparison will be implemented in the future if needed.

This is controled by the L</detect_no_diffs> option passed to the
base class constructor:

   my $self = $class->SUPER::new( ..., { detect_no_diffs => 1 } ) ;

and can be passed as an option to any rule.

=head1 AN EXAMPLE

Here's a real example, which will have to stand in for documentation
until further notice.  If you need more, mail me (barries@slaysys.com)
and get me to do something productive for a change.

This is a subclass that compiles a set of builtin rules at module
compilation time.  It declares a method for spawning the cvs command
first, then builds some rules.

   package Safari::Cvs::Make ;

   @ISA = qw( Slay::Maker ) ;

   use strict ;
   use IPC::Run ;

   sub cvs {
      my Safari::Cvs::Make $maker = shift ;

      my $stdout_to ;
      if ( $_[-1] =~ /^\s*>(.*)/ ) {
	 $stdout_to = $1 ;
	 pop ;
      }

      my $cvs_out ;
      run [ qw( cvs -q -l -z9 ), @_ ], \undef, \$cvs_out or die $! ;

      return $cvs_out ;
   }


   my $builtin_rules = Safari::Make->compile_rules(
      [  'meta/cvs_modules',
	 '=', sub {   ## The action that occurs when the rule fires.
	    ## We could just do the cvs co -c here, but many pservers don't
	    ## have that implemented.  so, check out the modules file and
	    ## parse it.
	    my ( $maker, $target ) = @_ ;
	    $maker->cvs( qw( checkout -p CVSROOT/modules ), ">$target" ) ;
	 },
      ],
      [ 'update',
	 ':' => sub {
	    my ( $maker ) = @_ ;

	    my $context = $maker->{CONTEXT} ;

	    my @modules ;

	    my %args = $context->request->args ;
	    if ( defined $args{modules} ) {
	       @modules = split( ',', $args{modules} ) ;
	    }
	    elsif ( defined $args{module} ) {
	       @modules = $args{module} ;
	    }
	    else {
	       eval {
	          ## A recursive make
		  $maker->make( 'meta/cvs_modules', { force => 1 } ) ;
	       } ;
	       if ( $@ ) {
		  warn $@ ;
	       }

	       if ( ! open( F, "<meta/cvs_modules" ) ) {
		  warn "$!: meta/cvs_modules, globbing" ;
		  @modules = map {
		     s{^meta/HEAD/}{}; $_
		  } grep {
		     -d $_
		  } glob( 'meta/HEAD/*' ) ;
	       }
	       else {
		  my $line ;
		  my %modules ;
		  while (<F>) {
		     next if /^\s*#|^\s*$/ ;
		     chomp ;
		     $line .= $_ ;
		     redo if $line =~ s{\\$}{} ;
		     $modules{$1} = 1 if $line =~ m{^\s*(\S+)} ;
		     $line = '' ;
		  }
		  close F ;
		  @modules = sort keys %modules ;
	       }
	    }

	    debug 'modules', \@modules ;
	    die "No modules found\n" unless @modules ;
	    return map { "source/HEAD/$_/CVS/" } @modules ;
	 },
	 '=' => sub {
	    my ( $maker, $target, $deps ) = @_ ;

	    my @dirs = map { s{/CVS/}{} ; $_ } @$deps ;

	    ## We go ahead and update after creating modules for a couple of
	    ## reasons:
	    ## 1. It's rare that we've just checked out a module
	    ## 2. It's simpler this way
	    ## 3. If we just created a *big* module, then we might need to
	    ## update anyway.

	    ## We set $logs{$filename} = 1 if we must replace the current log file,
	    ## or = 0 to just ensure that the log file is fetched.
	    my %logs ;
	    my $force_all ;

	    my $cwd = cwd() ;
	    for ( @dirs ) {
	       chdir $_ or die "$!: $_" ;
	       my $module = $_ ;
	       $module =~ s{.*/}{} ;
	       ## -P: Prune empty dirs
	       ## -d: Add new dirs that we don't have
	       for ( split /^/m, $maker->cvs( qw( update -d -P ) ) ) {
		  chomp ;
		  if ( /^[UP]\s+(.*)/ ) {
		     $logs{"meta/HEAD/$module/$1.log"} = 1 ;
		  }
		  elsif ( /^\?\s+(.*)/ ) {
		     my $log_file = "meta/HEAD/$module/$1.log" ;
		     eval {
			rmtree( [ $log_file ] ) ;
		     } ;
		     warn "Error removing ;$log_file'" if $@ ;
		  }
		  else {
		     warn "Unexpected CVS file mode: $_" ;
		  }
	       }
	       chdir $cwd or die "$!: $cwd" ;
	    }

	    for ( sort keys %logs ) {
	       $maker->make(
		  $_,
		  {
		     force => $force_all || $logs{$_}
		  }
	       ) ;
	    }

	 },
	 {
	    force => 1,   # Always remake this target
	 }
      ],
   ) ;





=cut

use strict ;
use Carp ;
use Cwd () ;
use File::Copy qw( copy move ) ;
use File::Spec::Unix ;
use Slay::MakerRule ;

use fields qw(
   COMMENTS
   ERRORS
   IN_QUEUE
   MADE_TARGETS
   OPTIONS
   OUTPUT
   RMAKE_STACK
   RULES
   QUEUE
) ;

## A few things that are cached for performane, so we're not always hittine
## the kernel up for filesystem data.
my %stat_cache ;
my $cwd_cache ;

## When chdir()ing to a symlink, these two vars save the symbolic and real
## values, so the symbolic one and the real one can both be checked when
## looking at targets with absolute paths.
my $sym_cwd_cache ;
my $real_cwd_cache ;

=head1 METHODS

=over

=item new

Constructor.

   my $rules = [
      # ...
   ] ;
   my $maker = Slay::Maker->new( $rules ) ;
   my $maker = Slay::Maker->new( $rules, { option => 1 } ) ;

options (which can also be defined on a per-rule basis) are:

=over

=item auto_create_dirs

Creates directories that targets will be created in before executing
a target's actions.

=item detect_no_diffs

Copy the target before executing a rule, then restore
the original modification and access times if the 
contents of the target are the same after the rule.

=item detect_no_size_change

Look for changes in the size of a file after executing
a rule and restore the original modification and access
times if it changes.

=item force

Always remake target, even if it does not appear to be
out of date

=back

Warning: options are not checked for spelling errors.

Options may be passed to new(), make(), build_queue(), and to rules themselves.
Options passed to make() or build_queue() take precedence over rules' options,
and rules' options take precedence over those passed to new().


=cut

sub new {
   my $proto = shift ;

   my $class = ref $proto || $proto ;

   my Slay::Maker $self ;
   {
      no strict 'refs' ;
      $self = bless [ \%{"$class\::FIELDS"} ], $class ;
   }

   my $options = ref $_[-1] eq 'HASH' ? pop : {} ;

   $self->{ERRORS} = [] ;
   $self->{OPTIONS} = $options ;
   $self->{RMAKE_STACK} = [] ;
   $self->{RULES} = [] ;
   $self->{QUEUE} = [] ;
   $self->{COMMENTS} = [] ;

   $self->add_rules( @{$self->builtin_rules} ) ;
   $self->add_rules( @_ ) ;

   return $self ;
}


=item add_rules

Add rules (compiled or not) to the rule base.

=cut


sub add_rules {
   my Slay::Maker $self = shift ;

   push @{$self->{RULES}}, @{$self->compile_rules( @_ )} ;
}


=item atime

This returns the atime of a file, reading from the stat cache if possible.

=cut

sub atime {
   my Slay::Maker $self = shift ;

   return ($self->stat( @_ ))[8] ;
}


=item build_queue

Builds a new queue of rules to be exec()ed to make a target

=cut

sub build_queue {
   my Slay::Maker $self = shift ;
   my $options = ref $_[-1] ? pop : {} ;
   
   $self->{QUEUE} = [] ;
   $self->{IN_QUEUE} = {} ;
   $self->{ERRORS} = [] ;

   $cwd_cache = undef ;

   $self->check_targets( @_, $options ) ;
}


=item builtin_rules

Returns [] by default.  This is provided so that subclasses may overload it
to provide sets of rules.  This is called by new() before adding any rules
passed to new().

=cut

sub builtin_rules { return [] }


=item canonpath

Cleans up the path, much like File::Spec::Unix::canonpath(), but also
removes name/.. constructs.

=cut

sub canonpath {
   my Slay::Maker $self = shift ;
   my ( $path ) = @_ ;

   my $trailing_slash = $path =~ m{/$} ;
   $path = File::Spec::Unix->canonpath( $path ) ;
   1 while $path =~ s{(^|/)[^/]+/\.\.(/|$)}{'/' if length "$1$2"}ge ;
   $path .= '/' if $trailing_slash ;
   return $path ;
}


=item chdir

Calls sytem's chdir(), die()s on failure, and uses the parameter as the
current directory.  The last is the main reason for this sub: if you chdir()
to a symbolic link, then we want to know the symbolic directory, not the
real one returned by cwd().

=cut

sub chdir {
   my Slay::Maker $self = shift ;
   my ( $path ) = @_ ;
   $path = $self->canonpath( $path ) ;
   chdir $path or die "$! chdir()ing to '$path'" ;
   my $cwd = cwd() ;
   if ( $path ne $cwd ) {
      $sym_cwd_cache = $path ;
      $real_cwd_cache = $cwd ;
   }
   else {
      $sym_cwd_cache = undef ;
      $real_cwd_cache = undef ;
   }
}


=item check_targets

Checks targets and adds them to queue if need be.  Does I<not> integrate
Slay::Maker options: this is left to the caller (usually the original
build_queue() call).

=cut

sub check_targets {
   my Slay::Maker $self = shift ;
   my $options = ref $_[-1] ? pop : {} ;
   my ( @targets ) = @_ ;
   
   for ( @targets ) {
      my ( $target, $r, $matches ) = $self->find_rule( $_, $options ) ;
      if ( ! defined $r ) {
	 push @{$self->{ERRORS}}, "Don't know how to make $_"
	    if ! -e $_ ;
	 next ;
      }
      $r->check( $self, $target, $matches, $options ) ;
   }
}


=item clear_caches

Clears the stat cache, so the filesystem must be reexamined.  Only needed
if Slay::Maker is being called repetetively.

=cut

sub clear_caches {
   my Slay::Maker $self = shift ;

   %stat_cache = () ;
   $cwd_cache = undef ;
   $sym_cwd_cache = undef ;
   $real_cwd_cache = undef ;
}


=item clear_stat

Clears the stat cache for a given path, so the next stat() on that path
will read the filesystem.

=cut

sub clear_stat {
   my Slay::Maker $self = shift ;

   my ( $path ) = @_ ;
   delete $stat_cache{$path} ;
}


=item compile_rules

Returns a rulebase compiled from the arguments.  Rules that are already
compiled are passed through unchanged.  This is a class method, so

   Slay::Maker->compile_rules( 
      [ 'a', [qw( b c )], 'cat b c > a' ],
      ...
   ) ;

can be used to compile a rulebase once at startup

=cut


sub compile_rules {
   my Slay::Maker $self = shift ;
   return [
      map {
	 ref $_ eq 'ARRAY' ? Slay::MakerRule->new( @$_ ) : $_ ;
      } @_ 
   ] ;
}


=item backup

Copies a file so that it can be restored later or checked for changes.

If the target will only ever be replaced by the make, then it will not be
altered in-place, and the C<move> option may be passed:

   $maker->backup( $target, { move => 1 } ) ;

If the target is an fiel which always changes size when it's changed,
you may pass the C<stat_only> option:

   $maker->backup( $target, { stat_only => 1 } ) ;

The return value can be passed to restore(), target_unchanged(),
and remove_backup().

=cut

sub backup {
   my $self = shift ;
   my $options = ref $_[-1] ? pop : {} ;
   my ( $target ) = @_ ;

   ## TODO: Detect collisions.
   my $temp_name = "$target.make_orig" ;
   if ( ! $options->{stat_only} && $self->e( $target ) ) {
      if ( $options->{move} ) {
	 print STDERR "Moving '$target' to '$temp_name'\n"
	    if $options->{debug} ;

	 move( $target, $temp_name ) ;
      }
      else {
	 print STDERR "Copying '$target' to '$temp_name'\n"
	    if $options->{debug} ;

	 copy( $target, $temp_name ) ;
      }
   }

   return {
      OPTIONS => $options,
      STAT    => [ $self->stat( $target ) ],
      FILE    => $target,
      BACKUP  => $temp_name,
   } ;
}


=item cwd

Returns the current working directory, from the cache if that's possible.

=cut

sub cwd {
   my Slay::Maker $self = shift ;
   return $cwd_cache if defined $cwd_cache ;
   $cwd_cache = Cwd::cwd() ;
   return $cwd_cache ;
}


=item e

Returns true if the file exists, but uses the stat_cache if possible.

=cut

sub e {
   my Slay::Maker $self = shift ;

   return defined $self->stat(@_) ;
}


=item exec_queue

Executes the queued commands.

=cut

sub exec_queue {
   my Slay::Maker $self = shift ;
   
   for ( @{$self->{QUEUE}} ) {
      my ( $target, $rule, @more ) = @$_ ;
      $self->clear_stat( $target ) ;
      push( @{$self->{OUTPUT}}, $rule->exec( $self, $target, @more ) ) ;
      push( @{$self->{MADE_TARGETS}}, $target ) ;
   }

   return @{$self->{MADE_TARGETS}} ;
}


=item find_rule

Given a target, finds a rule.

=cut

sub find_rule {
   my Slay::Maker $self = shift ;
   my $options = ref $_[-1] eq 'HASH' ? pop : {} ;

   my ( $target ) = @_ ;

   my $best_matches ;
   my $best_rule ;
   my $best_rank ;
   my $best_target ;

   my $cwd = $self->cwd() ;
   my $cwd_length = length $cwd ;

   ## If the target is absolute and is somewhere under the current dir, we
   ## generate a relative target in addition to the absolute one.
   my $rel_target ;

   $target = $self->canonpath( $target ) ;
   my $length = length $target ;

   if ( $length && substr( $target, 0, 1 ) eq '/' ) {
      if (  $length >= $cwd_length 
	 && substr( $target, 0, $cwd_length ) eq $cwd 
	 && (  $length == $cwd_length
	    || substr( $target, $cwd_length, 1 ) eq '/' 
	 )
      ) {
	  $rel_target = substr( $target, $cwd_length ) ;
	  $rel_target =~ s{^/+}{} ;
      }
      elsif ( defined $sym_cwd_cache && $real_cwd_cache eq $cwd ) {
         $cwd_length = length( $sym_cwd_cache ) ;
	 if (  $length >= $cwd_length 
	    && substr( $target, 0, $cwd_length ) eq $sym_cwd_cache
	    && (  $length == $cwd_length
	       || substr( $target, $cwd_length, 1 ) eq '/'
	    )
	 ) {
	     $rel_target = substr( $target, $cwd_length ) ;
	     $rel_target =~ s{^/+}{} ;
	 }
      }
   }

   for ( @{$self->{RULES}} ) {
      my ( $rank, $matches ) = $_->matches( $target, $options ) ;
      ( $best_target, $best_rule, $best_rank, $best_matches ) =
         ( $target, $_, $rank, $matches )
	 if $rank && ( !defined $best_rank || $rank > $best_rank ) ;
      if ( defined $rel_target ) {
	 my ( $rank, $matches ) = $_->matches( $rel_target, $options ) ;
	 ( $best_target, $best_rule, $best_rank, $best_matches ) =
	    ( $rel_target, $_, $rank, $matches )
	    if $rank && ( !defined $best_rank || $rank > $best_rank ) ;
      }
   }

   return ( $best_target, $best_rule, $best_matches ) ;
}


=item make

Makes one or more target(s) if it's out of date.  Throws exceptions if
the make fails.  May partially make targets.

=cut


sub make {
   my Slay::Maker $self = shift ;
   my $options = ref $_[-1] ? pop : {} ;

   if ( ! $self->make_level ) {
      $self->{COMMENTS} = []  ;
      $self->{OUTPUT}   = []  ;
      $self->{MADE_TARGETS} = [] ;
   }

   $self->recurse_in ;

   eval {
      $self->build_queue( @_, $options ) ;

      croak join( '', @{$self->{ERRORS}} ) if @{$self->{ERRORS}} ;

#   push(
#      @{$self->{COMMENTS}},
#      join( ', ', @_ ) . " up to date"
#   )  unless $self->queue_size ;

      $self->exec_queue( $options ) ;
   } ;
   my $a = $@ ;

   eval {
      $self->recurse_out ;
   } ;

   if ( $a ) {
      $@ = $a ;
      die ;
   }

   print STDERR map { "$_\n" } @{$self->{COMMENTS}}
      if $options->{debug} && ! $self->make_level ;

   croak join( '', @{$self->{ERRORS}} ) if @{$self->{ERRORS}} ;

   return @{$self->{MADE_TARGETS}} ;
}


=item make_level

Returns 0 if make() has not been called (well, actually, if recurse_in()
has not been called).  Returns number of recursive calls otherwise, so this
is equal to 1 when making something but not recursing.

=cut

sub make_level {
   my Slay::Maker $self = shift ;
   return scalar( @{$self->{RMAKE_STACK}} ) ;
}


=item mtime

This returns the mtime of a file, reading from the stat cache if possible.

=cut

sub mtime {
   my Slay::Maker $self = shift ;

   return ($self->stat( @_ ))[9] ;
}


=item options

Sets / gets a reference to the options hash.

=cut

sub options {
   my Slay::Maker $self = shift ;
   $self->{OPTIONS} = shift if @_ ;
   return $self->{OPTIONS} ;
}


sub output {
   my Slay::Maker $self = shift ;
   return wantarray ? @{$self->{OUTPUT}} : join( '', @{$self->{OUTPUT}} ) ;
}


=item push

Adds a ( target, rule ) tuple to the exec queue.  Will not add the same target
twice.

=cut

sub push {
   my Slay::Maker $self = shift ;
   my ( $target, $rule ) = @_ ;

   if ( $self->{IN_QUEUE}->{$target} ) {
      push @{$self->{COMMENTS}}, "Only making $target once" ;
      return ;
   }

   push @{$self->{QUEUE}}, [ @_ ] ;
   $self->{IN_QUEUE}->{$target} = $rule ;
}


=item recurse_in

Sets up for a recursive make.  Called automatically by make() if make() is
already running.

=cut

sub recurse_in {
   my ( $self ) = @_ ;
   CORE::push @{$self->{RMAKE_STACK}}, [ @$self{'QUEUE','IN_QUEUE'} ] ;
   $self->{QUEUE} = [] ;
   $self->{IN_QUEUE} = {} ;
}


=item recurse_out

Restored after a recursive make.  Called automatically by make() if make() is
already running.

=cut

sub recurse_out {
   my Slay::Maker $self = shift ;
   @$self{'QUEUE','IN_QUEUE'} = @{pop @{$self->{RMAKE_STACK}}} ;
}


=item queue_size

Number of rules that need to be made.

=cut

sub queue_size {
   my Slay::Maker $self = shift ;
   scalar( @{$self->{QUEUE}} ) ;
}


=item remove_backup

   my $backup = $maker->backup( $target ) ;
   ## ... 
   $maker->remove_backup(
      $backup,
      {
	 restore_if_unchanged => 1,
	 deps                 => \@deps,
      }
   ) ;

Removes a backup of the target created with backup_target().

=cut

sub remove_backup {
   my $self = shift ;

   my $options = ref $_[-1] ? pop : {} ;

   my ( $backup ) = @_ ;

   $self->restore( $backup, $options )
      if $options->{restore_if_unchanged} && $self->target_unchanged( $backup );

   if ( defined $backup->{BACKUP} ) {
      if ( -e $backup->{BACKUP} ) {
	 print STDERR "Unlinking $backup->{BACKUP}.\n"
	    if $options->{debug} || $backup->{OPTIONS}->{debug} ;

	 unlink $backup->{BACKUP} or warn "$!: $backup->{BACKUP}" ;
      }
      else {
	 print STDERR "Can't unlink $backup->{BACKUP}: it's not present.\n"
	    if $options->{debug} || $backup->{OPTIONS}->{debug} ;
      }
   }
}


=item replace_rules

Replaces the rule for a target (or targets).  The targets passed in must
exactly match those of the rule to be replaced.

=cut

sub replace_rules {
   my Slay::Maker $self = shift ;
   for my $new_rule ( @{$self->compile_rules( @_ )} ) {
      my $targets = $new_rule->targets ;

      for ( @{$self->{RULES}} ) {
	 if ( $targets eq $_->targets ) {
	    $_ = $new_rule ;
	    return ;
	 }
      }
      $self->add_rules( $new_rule ) ;
   }
}


=item restore

   my $backup = $maker->backup( $target, { move => 1 } ) ;
   ## Try to recreate target, setting $error on error
   $maker->restore( $backup )
      if $error ;
   $maker->restore( $backup, { deps => \@deps } )
      if ! $error && $maker->target_unchanged( $backup ) ;
   $maker->remove_backup( $backup ) ;

Note that you only need this in case of an error.  You can pass the
restore_if_unchanged => 1 and deps => \@deps options to 
remove_backup().

When backup() has been called, it's return value can be passed
to restore_target() to restore the original target, timestamps and all.

NOTE: restoring a target that's not changed is likely to cuase it to
be remade every time once a dependancy's timestamp becomes more recent.
The C<deps> option allows the timestamps to be set to the newest of
the original timestamps and the dependencies' timestamps.  This should
not be done if there was an error generating the file.

=cut

sub restore {
   my $self = shift ;
   my $options = ref $_[-1] ? pop : {} ;
   my ( $backup ) = @_ ;

   if ( -e $backup->{BACKUP} ) {
      print STDERR "Restoring '$backup->{BACKUP}' to '$backup->{FILE}'\n"
	 if $options->{debug} || $backup->{OPTIONS}->{debug} ;
      move( $backup->{BACKUP}, $backup->{FILE} )
   }

   if ( defined $options->{deps} ) {
      my ( $atime, $mtime ) = ( 0, 0 ) ;
      ( $atime, $mtime ) = @{$backup->{STAT}}[8,9]
	 if @{$backup->{STAT}} ;

      for ( @{$options->{deps}} ) {
	 my $a = $self->atime( $_ ) ;
	 $atime = $a if defined $a && $a > $atime ;
	 my $m = $self->mtime( $_ ) ;
	 $mtime = $m if defined $m && $m > $mtime ;
      }
      utime $atime, $mtime, $backup->{FILE} ;
   }

   $backup->{BACKUP} = undef ;
}

=item rules

Gets or replaces the rules list

=cut

sub rules {
   my Slay::Maker $self = shift ;

   if ( @_ ) {
      $self->{RULES} = [] ;
      $self->add_rules( @_ ) ;
   }

   return wantarray? @{$self->{RULES}} : $self->{RULES} ;
}

=item size

This returns the size of a file, reading from the stat cache if possible.

=cut

sub size {
   my Slay::Maker $self = shift ;

   return ($self->stat( @_ ))[7] ;
}


=item stat

Looks in the stat cache for the stat() results for a path.  If not found,
fills the cache.  The cache is shared between all instances of this class,
and may be cleared using clear_stat_cache().

=cut

sub stat {
   my Slay::Maker $self = shift ;
   my ( $path ) = @_ ;

   return @{$stat_cache{$path}}
      if defined $stat_cache{$path} ;

   my @stats = stat $path ;
   $stat_cache{$path} = \@stats if @stats ;

   return @{$stat_cache{$path}}
      if defined $stat_cache{$path} ;

   return () ;
}

=item target_unchanged

Takes the result of backup_target() and checks to see if the target has
been changed or removed.

=cut

sub target_unchanged {
   my $self = shift ;
   my ( $context ) = @_ ;

   my $target = $context->{FILE} ;

   $self->clear_stat( $target ) ;

   ## See if the file disappeared or appeared.  This is an exclusive-or.
   return 0
      if @{$context->{STAT}}
         ? ! $self->e( $target )
	 :   $self->e( $target ) ;

   ## It's unchanged if neither existed.
   return 1 unless @{$context->{STAT}} ;

   ## It's not unchanged if it's size changed
   return 0
      if $context->{STAT}->[7] ne $self->size( $target ) ;

   ## TODO: Use Diff.pm to do this.  Also, investigate using MD5 as an
   ## alternative to diffing, to save the copy operation.
   return 0
      if ! $context->{OPTIONS}->{stat_only} &&
         length `diff --brief "$context->{BACKUP}" "$target"` ;

   return 1 ;
}


=item back

=head1 TODO

=over

=item *

Propogate effects of restored timestamps.

If a target has it's timestamps restored as a result of detecting no
change (see options detect_no_size_change and detect_no_diffs), then
there may well be no need to actually execute later rules. 

One way to do this is to re-check the mtime dependencies when rebuilding.
Another is to subscribe later items in the queue to earlier items and have
the earlier items set a flag that tells the later items to go ahead and
execute.  Items could flag themselves to execute regardless, which we might
want to do if a dependancy is not present when make is run.

=item *

Don't really call diff(1) for detect_no_diffs

=back

=head1 AUTHOR

Barrie Slaymaker <barries@slaysys.com>

=head1 LICENSE

Copyright 2000, R. Barrie Slaymaker, Jr., All Rights Reserved.

That being said, do what you will with this code, it's completely free.

Please let me know of any improvements so I can have the option of folding
them back in to the original.

=cut

1 ;
