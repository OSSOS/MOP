#!/usr/bin/perl -w

use strict;

my $sendmail = "/usr/sbin/sendmail -t";
my $reply_to = "Reply-to: jj.kavelaars\@nrc.gc.ca\n";
my $subject  = "Subject: New exposures are available\n";
my $content  = "Some new expoures have arrived... you should do something\n";
my $send_to  = "To: jj.kavelaars\@nrc.gc.ca\n";


open (SENDMAIL, "|$sendmail");
print SENDMAIL $reply_to;
print SENDMAIL $subject;
print SENDMAIL $send_to;
print SENDMAIL "Content-type: text/plain\n\n";
print SENDMAIL $content;
close(SENDMAIL);

