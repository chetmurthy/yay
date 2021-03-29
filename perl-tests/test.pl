#!/usr/bin/perl

use strict ;
use Data::Dumper ;
use YAML::PP;

{
  my $ypp = YAML::PP->new;
  my @l = <ARGV>;
  my $l = join('',@l) ;
  my @docs = $ypp->load_string($l) ;
  print "================\n" ;
  print Dumper(\@docs),"\n";
  print "================\n" ;
  print $ypp->dump_string(@docs),"\n" ;
}
