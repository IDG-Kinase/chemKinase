#!/usr/bin/perl -w

use Text::CSV::Simple;
use Data::Dumper;
use File::Copy;

my $parser = Text::CSV::Simple->new;
my @data = $parser->read_file('HMS-LINCS_KinomeScan_Datasets_2018-01-18.csv');

#get rid of header column
shift @data;

mkdir('Data Files');

for my $line (@data) {
  my $wget_cmd = "wget 'http://lincs.hms.harvard.edu/db/datasets/@$line[4]/results?search=&output_type=.csv'";
  system($wget_cmd);
  move("results?search=&output_type=.csv","Data Files/" .@$line[4] . ".csv");
}

