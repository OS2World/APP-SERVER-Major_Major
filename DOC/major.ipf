:userdoc.
:title.Major Major documentation
:docprof toc=1234.

.***********************************
.*   INTRODUCTION
.***********************************

:h1.Introduction

:p.
Major Major is a mailing list manager. It allows you to set up
one or more mailing lists, and it allows people to subscribe to those
lists. When mail comes in for the list, Major Major sends a copy
to everyone on the list.
:p.
It is distributed as open-source freeware subject to the GNU GPL
licence. You may obtain source code from the place where you
downloaded this package.

:p.This documentation is for version 3.1.

:p.
:hp2.Disclaimer of Warranty:ehp2.

:sl compact.
:li.
:hp1.
This Product is provided "as-is", without warranty of any
kind, either expressed or implied, including, but not limited to,
the implied warranties of merchantability and fitness for a
particular purpose. The entire risk as to the quality and
performance of the Product is with you. Should the Product prove
defective, the full cost of repair, servicing, or correction lies
with you.
:ehp1.
:esl.

:p.
The author of Major Major is Peter Moylan, peter@pmoylan.org.

:p.
The latest version of Major Major is normally kept at http&colon.&slash.&slash.www.pmoylan.org/software.
Information about other software on this site may also be found on that page.

:p.
:hp2.Getting information about new versions:ehp2.

:p.
You can, if you wish, join a mailing list for announcements about
new releases of my software. The mailing list majormajor-list@os2voice.org is a
general forum for discussions and questions about Major Major. To join
this list, send an
e-mail to majormajor@os2voice.org. The subject line is not
important and may be anything. In the body of the message, put the
lines
:xmp.

       subscribe majormajor-list
       end
:exmp.

:p.To have yourself removed from a list, send a similar e-mail but
using the command "unsubscribe" instead of "subscribe".

:p.
:hp2.Finding out which version you have:ehp2.

:p.If you have lost track of which version of Major Major you have, open
an OS/2 command window and type the command
:xmp.

       bldlevel major.exe
:exmp.

:p.The "bldlevel" command is an OS/2 feature, not a Major Major feature.

.***********************************
.*   PREREQUISITES
.***********************************

:h1 id=prerequisites.Prerequisites

:hp2.Prerequisites:ehp2.

:p.This software assumes that both INIDATA.DLL and XDS230M.DLL are in your
LIBPATH. If, when trying to run Major.exe, you get a message like
"The system cannot find the file XDS230M", you must install INIData,
version 1.1 or later. INIData can be found at the same web or FTP site as where
you found the Major Major zip file.

.***********************************
.*   HOW IT WORKS
.***********************************

:h1.How it works

:hp2.What is a mailing list?:ehp2.

:p.
A mailing list is similar to a newsgroup, except that it uses e-mail
rather than Usenet to transport the messages. People join the list
by mailing a "subscribe" command to the mailing list manager. If
they later want to leave the list, they send in an "unsubscribe"
command.
:p.
The name of the list is an e-mail address. That is, it has the form
"listname@domain", where "domain" is the mail domain for the mail
server that receives mail for the list. You send a message to the
list by mailing it to that address. When it is received, the list
manager sees it and sends copies to everyone who is subscribed to
the list. In other words, when you send an e-mail to the list you
are actually sending it to multiple people.

:p.:hp2.Can I use it to send junk mail?:ehp2.

:p.
You could try, but it would not be a good idea. Mailing lists are
superficially similar to junk mail software, in that they both
send mail to multiple recipients, but there are two crucial
differences&colon.
:ul.
:li.Major Major, like all ethical mailing list software, has
an 'unsubscribe' command, and there is no way to disable this. If
you try to use this to send junk mail, your victims will be able
to remove themselves from the list.
:li.Mailing list processors, unlike junk mail software, do not
have any way for senders to hide their identity.
:eul.

:p.
In any case, I reserve the right to take punitive action against
any Major Major user who uses this software to send junk mail. The
possible punitive actions include, but are not restricted to,
including the sender's address(es) in filters included in any
software that I develop, and supplying such addresses to other
developers of anti-junk software and to maintainers of e-mail
blacklists. The punitive actions also include any
measures that I might devise to ensure that Major Major will not
work correctly for junk mailers.

:p.:hp2.Major Major and its interaction with the mail server:ehp2.

:p.
A mail server consists of two parts. (These might be two separate
pieces of software, or they might be integrated in a single package.)
One part is the SMTP software, which is the software that looks after
transferring mail between the source and destination machines. The
other part is the "Post Office" server, through which users pick up
their mail after it has arrived. Major Major assumes that the
Post Office server is a POP3 server, which at least for now is the
most common kind of Post Office server.

:p.
One way to implement a mailing list manager would be to make it an
integral part of the mail server software. I haven't done it this
way, however, because I dislike large integrated software packages.
Rather than have one giant program that does everything, I prefer
to have many small programs, each of which does one thing and
does it well. That gives a cleaner design, with less chance of
undetected software errors. It also means that the users can
"mix and match" their software, according to their own preferences,
without having to get it all from the same supplier.

:p.
As a result of this, Major Major is :hp1.not:ehp1.  itself a mail
server. It assumes that a mail server is already installed - not
necessarily on the same machine - and
it interacts with that mail server. When mailing list mail arrives
at the Post Office machine, Major Major gets it from the POP3
server. Then Major Major decides who it should be forwarded to,
and it sends it back to the SMTP server to be delivered.

:p.
When you set up a mailing list, you specify to Major Major how
often it should check for mail. Most mailing lists don't need a
rapid response, so it would be sufficient to check them once an
hour or even once a day. In the case of a discussion list that
has lively back-and-forth discussions, you might want to cut the
sampling interval down to a few minutes. In an extreme case
you might even reduce it to a few seconds; but that is not
recommended, because it puts an extra load on your processor and on the mail server.

:p.
Major Major can handle multiple mailing lists, and you can set
different sampling intervals for different lists.

:p.:hp2.Special case: the interaction with Weasel:ehp2.

:p.
If you are using the Weasel mail server, and it is running on the
same machine as Major Major, a shortcut is possible. In this
case, Major Major doesn't need to use the POP3 and SMTP protocols
to send and receive mail. Instead, it looks directly into the
disk directories where Weasel stores the mail. This removes
some overhead from the operation, because you don't have as much
network traffic.

:p.
It can also mean a more timely response. Major Major has a way of
knowing when Weasel has received new mail, provided that it is
running on the same machine, so it will distribute mail as soon as
it arrives rather than waiting for the specified check interval.
Sometimes we will be forced to wait for the check interval anyway,
because of imperfections in the OS/2 implementation of event
semaphores, but most of the time you will get a response within
a very short time.

.***********************************
.*   LANGUAGE SUPPORT
.***********************************

:h1.Language support

:hp2.Language support:ehp2.

:p.Major Major supports multiple languages, including the use of
different languages for different mailing lists on the same server.
A language is specified by a code, for example 'en' for English
and 'fr' for French. The usual convention is to use a two-letter
code, although you can go up to 31 letters if you wish. To support
language 'xy', you need two special files in the same directory as
Major.exe.
:ul.
:li.The file MM.xy.lng defines a set of messages used by Major
Major itself (for log messages, replies to users, etc.).
:li.The file Admin.xy.lng is used by the program Admin.exe, whose
function is to control all configuration details of Major Major.
:eul.

:p.In addition, the 'Canned' directory contains a collection of
files containing things like responses to the 'help' command.
As supplied, these are organised into subdirectories according to
language. You can either use these files as supplied, or make
your own versions. The directory layout supplied is not compulsory;
if you prefer, you can put those message files elsewhere.

:note text='Note 1.'.If you do modify the 'Canned' files, make sure that you
either give them different names, or put them in a different
directory. Otherwise, there is a risk that your customised versions
will be overwritten the next time you upgrade to a new version
of Major Major.

:note text='Note 2.'.If you have just upgraded from a version of Major
Major that did not have multilingual support, you will notice that
the files that used to be in the 'Canned' directory have now been
moved to subdirectories of 'Canned'. You may now delete the old files
in the top-level 'Canned' directory, unless of course you have added
files there that you wish to keep.

:p.If you want to add support for a new language, the easiest way is
to copy all the files for an existing supported language and then translate them. The format
should be obvious from looking at the files. If you want to send
those translations to me, I would be pleased to include them in
future distributions of Major Major. At present there is full
support for Danish, Dutch, English, Esperanto, French, German,
and Italian, and partial support for
Spanish, but I would like to expand this support
to include any language for which someone is prepared to do a
translation. (Remark: I don't have any experience with right-to-left
languages or DBCS languages, so I don't know whether the software
will handle those.)

:p.:hp2.Acknowledgements:ehp2.

:p.I would like to express my appreciation to those people who
have already supplied translation help&colon. Jose De Dona,
Marion Gevers, Christian Hennecke, Allan Holm, Jonathan Moylan,
Javier Orejarena, Fritz Schori, Piersante Sestini, Michel Such, and
Ronald van Ovost.

.***********************************
.*   INSTALLATION
.***********************************

:h1.Installation
:hp2.Installation:ehp2.
:p.
See also :link reftype=hd refid=deinstall.De-installation:elink..

:p.You should have received this package in the form of a zip file.
The first step is to unzip the file into a directory of your choice.
(Presumably you have already done this.) You will find that you
have executable files Major.exe and Admin.exe, and a few other
files.

:note.Do not install Major Major in the same directory as
Weasel, since this will cause some Weasel files to be overwritten.
As a general rule, it's not a good idea to put multiple programs
into the same directory, except in special cases where you've checked
that there will be no file name conflicts.

:p.Admin.exe sets up the configurable parameters, so you should
run it first. The required settings should be obvious. If
not, you can refer to the :link reftype=hd refid=Configuration.configuration:elink.
section of this manual. The first time you run Admin.exe, there
could be a delay while it queries the tcp/ip software for the
hostname of this machine. After the first run this delay will not
occur.

:p.Major.exe is the actual mailing list manager, and you leave
it running all the time. Ultimately you will probably want to
run it minimized or detached, but initially it is a good idea
to run it directly on your desktop, so that you can verify that
it is doing what you expect.

:p.It is a good idea to run DumpINI (see later) after your installation
is up and working, and thereafter at monthly intervals, to ensure that
you have a backup of the configuration including addresses of all
the list subscribers.

:p.Major.exe has three optional parameters. If you start it with the command
:xmp.

           MAJOR X

:exmp.
then the transaction log might contain more detail than usual. The
precise detail depends on what I need to test. This feature is for
testing and troubleshooting, so most users won't need to use it.
This parameter will eventually become obsolete, because there is now
an option in the Admin notebook to enable extra logging.

:p.Major.exe takes its configuration data from either Major.INI or Major.TNI,
depending on which of these two files is present.

(These configuration files are created by the
:link reftype=hd refid=Configuration.Admin:elink. program.)

If neither one is present, or if both are, it chooses the option specified the last time
ADMIN -I or ADMIN -T was run. If none of these rules apply, then it uses Major.INI.

You can overrule this decision with an explicit parameter. If you start it with the command
:xmp.

           MAJOR I

:exmp.
then it will take its configurable parameters from the file MAJOR.INI.
If you start it with the command
:xmp.

           MAJOR T

:exmp.
then it will take its configurable parameters from the file MAJOR.TNI.

:p.If you need to stop Major.exe, type CTRL/C. Alternatively,
perform a signal on the global event semaphore \SEM32\MAJOR\SHUTDOWN.
A process killer, e.g. the second button in the WarpCenter, will also work, but
the CTRL/C or event semaphore methods are preferred because they make
Major Major tidy up and exit cleanly.

:p.The command file MakeFolder.cmd is optional. If you run it,
it will create a desktop folder to let you work with Major Major
without having to open the directory in which it is installed.

:p.Of course, Major Major is not going to do anything interesting
until you have created one or more mailing lists, and this includes
creating the e-mail accounts for those lists. Those details
are covered in the following sections.

.***********************************
.*   DEINSTALLATION
.***********************************

:h1 id=deinstall.De-installation
:hp2.De-installation:ehp2.
:p.
Major Major does not tamper with CONFIG.SYS or with other system files.
If you decide that you don't want to keep Major Major, simply delete
the directory into which you installed it.

:p.You should probably also go to your mail server and close down
the e-mail accounts that you were using for Major Major, unless
of course you still need those accounts.

.***********************************
.*   SETTING UP THE E-MAIL ACCOUNTS
.***********************************

:h1.Setting up the e-mail accounts
:hp2.Setting up the e-mail accounts:ehp2.

:p.Major Major needs one e-mail account for itself, plus one account
for each mailing list it is managing. The account that Major Major
reserves for itself is the one that receives the administrative
commands such as "subscribe" and "unsubscribe". The other accounts
are the ones that receive the mailing list traffic.

:p.To provide these accounts, you have basically two choices.
:ul.
:li.You can use an existing e-mail server, and ask the manager of
that server to create the accounts for you. You might have to pay
for these accounts, depending on your service provider's rules.
:li.Alternatively, you can become your own postmaster by installing
a mail server on your own computer. To do this, you must of course
have enough network connectivity to send and receive mail.
:eul.

:p.Major Major does not have to be physically on the same machine
where the mail server is running.

:p.:hp2.Note&colon.:ehp2. The draft standard RFC2142 states that
:xmp.

   Mailing lists have an administrative mailbox name to which add/drop
   requests and other meta-queries can be sent.

   For a mailing list whose submission mailbox name is:

      <LIST@DOMAIN>

   there MUST be the administrative mailbox name:

      <LIST-REQUEST@DOMAIN>
:exmp.

:p.A strict conformance to this rule would require you to create 2N+1
mail accounts to service N mailing lists, rather than the N+1 accounts
as described above. Major Major does not enforce the above rule, on
the grounds that this would create extra costs for some users.

:p.If, however, you do decide to conform to the RFC2142 rule, you
can do so by setting up aliases or redirections (depending on what
your mail server supports) so that the "request" mail is sent on
to the Major Major administrator. For example, if you have a list
called MYLIST@DOMAIN, then you can create an e-mail alias called
MYLIST-REQUEST@DOMAIN, whose mail is then redirected to
MAJORMAJOR@DOMAIN. (This assumes that you are using the default
name MAJORMAJOR as the administrator account name. You are free
to choose some other name.)

:p.If you are supporting only one list, then of course you can comply
with RFC2142 by choosing an administrator name of the form MYLIST-REQUEST.

.***********************************
.*   CONFIGURATION
.***********************************

:h1 id=configuration.Configuration

:p.:hp2.The Admin program:ehp2.

:p.The program called Admin.exe is what you use to configure Major Major.
The configuration parameters are stored in a file called either Major.INI
or Major.TNI, depending on which of these is physically present in the
working directory. (If neither file exists, or if both exist, Admin chooses
the one that was last selected with Admin -I or Admin -T. If that rule
fails, it chooses Major.INI.) You may explicitly override that decision
with the 'I' or 'T' parameters - see below.

:p.In effect, Admin.exe is an editor for that configuration file.

:p.Major.INI, if it exists, is a binary file whose format is natively
supported by OS/2. Major.TNI, if it exists, is a human-readable file
that contains the same information. The INI format is more efficient, but
there is a catch: INI files are stored in a region of main memory that, for
historical reasons, has limited size, and in modern OS/2 configurations
there is a risk of memory overflow in that region, which can cause INI
file corruption. If you discover that Major.INI is being corrupted when
the system is busy, you should switch to the TNI format. Otherwise, the
INI format is a good choice.

:p.When you run Admin, you will get a small screen window with Local/Remote
radio buttons and three pushbuttons. The Remote option is for remote
configuration, which is described in a
:link reftype=hd refid=remoteconfig.separate section:elink.. Normally you
should choose the Local option, which will edit the Major.INI or Major.TNI file that
resides in the same directory as ADMIN.EXE. Click on the "GO" button to
start the editing.

:p.If you start Admin with the command
:xmp.            admin -I

:exmp.
then Admin will edit the file MAJOR.INI.
If you start Admin with the command
:xmp.            admin -T

:exmp.
then Admin will edit the file MAJOR.TNI.
This overrides the rules mentioned above about which one to choose.
The 'I' and 'T'
options can, if desired, be combined with the 'L', 'R', and 'G' options
documented below.

:p.If you start Admin with the command
:xmp.            admin -L

:exmp.
then you will bypass the small opening screen and go directly into local editing.

:p.If you start Admin with the command
:xmp.            admin -R

:exmp.
then you will bypass the small opening screen and go directly into remote editing.

:p.If you start Admin with the command
:xmp.            admin -G

:exmp.
then you don't get the small opening screen, and the Local/Remote option
has whatever value it had the last time you ran Admin.

:p.More than one option can be specified, but of course if you try to
specify more than one of 'L', 'R', and 'G' the result is undefined.
Similarly, specifying both 'I' and 'T' gives undefined behaviour.
These option characters are case-independent, e.g. 'r' means the same as
'R'. In the present version of Admin the '-' character is ignored, so it
does not matter if you omit it. This could change if a future version of Admin
has command-line parameters more than one character long.

:p.After passing the small opening screen, you get a notebook with the
following pages.

:ul.
:li.:link reftype=hd refid=Basic.Basic:elink. - some overall program settings.
:li.:link reftype=hd refid=Files.Files:elink. - where to put log messages and list archives.
:li.:link reftype=hd refid=Admin.Admin:elink. - settings for the Major Major administrator.
:li.:link reftype=hd refid=Messages.Messages:elink. - the file names for some standard messages
:li.:link reftype=hd refid=Lists.Lists:elink. - creation and deletion of mailing lists.
:li.:link reftype=hd refid=About.About:elink. - identifies the program version.
:eul.

:h2 id=configurationfonts.Changing fonts
:hp2.Changing fonts:ehp2.

:p.If you want to change the font used for the Admin notebook pages, drop
a new font from the Font Palette to a page. The change will affect every
page in that notebook. You can use the same method to change the fonts on
the notebook tabs.

:p.Note that the font must be dropped onto the "background" part of the
notebook page. If you drop a new font onto an individual entry field, for
example, the font will change for the current session but it will not
be remembered the next time you open up the notebook. I felt that it was
unlikely that anyone would want a whole lot of different fonts for different
fields.

.***********************************
.*   BASIC
.***********************************

:h2 id=Basic.Basic settings
:hp2.Basic settings:ehp2.

:p.Changes made on this page will not have any effect until you restart
Major Major.

:p.:hp2.Server and domain:ehp2.

:p.The first thing you have to specify on this page is whether you are using
the Weasel mail server, running on the same machine as Major Major, as your
mail server. You do this by selecting one of two radio buttons. If you choose
"Weasel on this machine", Major Major will send and receive mail by direct
manipulation of the Weasel mail directories. If you choose "Other mail server",
Major Major will use the SMTP and POP3 protocols to send and receive mail.

:p.The mail domain name is what comes after the '@' sign in the e-mail address
used by Major Major. Sometimes this is just the hostname of the machine on which
the mail server is running, but it is also possible for a single mail domain to
have multiple mail servers; in this case the mail domain name is, in effect, an
identifier for the group.

:p.The main use for this field within Major Major is in constructing the
"Reply-To" header line that is part of every e-mail sent out. In the
case where you have selected "Weasel on this machine", and Weasel is
configured to support multiple domains, the mail domain name is also used
to work out which directory the mail is stored in.

:p.:hp2.Batch size limit:ehp2.

:p.The SMTP mail standards specify that a mail server must be able to
handle at least 100 recipients for a single mail message. It turns out
that some mail servers do not respect this requirement, so by default
Major Major breaks up outgoing mail into groups of not more than 50
recipients. This configuration setting lets you change the default.

:p.Setting this limit too low will cause extra network traffic, so
ideally you should set it somewhere between 50 and 100. To avoid
overloading your mail server (or your ISP's mail server), you should
reduce this limit only if you are having problems with the receiving servers.

:p.:hp2.Root directory for mail:ehp2.

:p.If you see an entry with this label, you have an obsolete version of
Admin.EXE. This field is no longer used with the current version of
Major Major.

:p.:hp2.Weasel directory:ehp2.

:p.This is needed only if you have selected the "Weasel on this machine" option. It
should specify the path name (which may be relative or absolute) of the disk directory that contains the file
called WEASEL.INI. Major Major uses that file to work out where mail is stored
on the disk.

:p.:hp2.SMTP and POP3 servers:ehp2.

:p.These must be specified if you have selected the "Other mail server" option.
The SMTP server is used for sending mail out, and the POP3 server is for incoming
mail. Often these are on the same machine, but this is not compulsory.

:p.For each server you have to specify a hostname and a port number. The
standard port number for an SMTP server is 25, and the standard port number for
a POP3 server is 110. Do not use different port numbers unless the postmaster
tells you to.

:p.Some SMTP servers require you to provide
:link reftype=hd refid=SMTPauthentication.authentication:elink.
before sending outgoing mail.
To enable this, select the checkbox under the SMTP hostname field.

:p.If you later want to change these details, click on the "use SMTP
authentication" checkbox twice: once to disable it, and once more to
enable it again.

:p.:hp2.Log detail:ehp2.

:p.The "Log detail" checkbox lets you control how much detail goes into
the log. (But it has no effect unless you enable logging on the
:link reftype=hd refid=Files.Files:elink. page.)
If you leave this button unchecked you get a low level of logging.
If you select it, a bit more detail goes into the log. Exactly what is
included can vary between versions of Major Major.

.***********************************
.*   POP-BEFORE-SMTP AUTHENTICATION
.***********************************

:h3 id=SMTPauthentication.Authentication for outgoing mail
:hp2.Authentication for outgoing mail:ehp2.

:p.As a protection against system abuse, the operators of most mail servers
put restrictions on who may send out mail via their servers. The usual
arrangement is that an ISP will accept mail for forward delivery only
from its own customers. That means that it must apply some sort of
"legitimacy" check on the sender of mail to be forwarded. Obviously
this will include a check on mail sent to it from Major Major.

:p.One common form of legitimacy check is a check on the IP address
of the sender. If this is what is being done in your case, and Major
Major is running on a machine that the server considers to be legitimate,
you can ignore this section. The mail from Major Major should be
going out without problem.

:p.An alternative approach, used by some mail servers, is to require the client
to provide some sort of identification before sending out mail. Typically this
would be done by giving the username and password (possibly in an
encrypted form) for an existing POP mail account. Major Major supports
two kinds of authentication; your mail system administrator should have
told you which of these two, if any, you should be using.

:p.:hp2.The SMTP AUTH method of authentication:ehp2.

:p.The SMTP AUTH command is an extension to the SMTP standard, by which
you supply your username and password to the SMTP server each time
you log in to send out mail. There are actually several different
variants of this command, at different levels of security. Major
Major supports three of the variants, called PLAIN, LOGIN, and
CRAM-MD5. You do not have to specify which variant to use, because
Major Major can work out which of these the server supports, and
it automatically chooses the most secure option available. All you
have to do is supply the username and password.

:p.:hp2.The POP-before-SMTP authentication method:ehp2.

:p.Some mail server installations use a different rule&colon. they
require you to send your outgoing mail only after fetching your
incoming mail from the POP server. (The reason for this is that a POP
login requires a correct password, whereas the standard SMTP protocol
- without the SMTP AUTH extension - does not
use passwords.) Normally this is not an inconvenience to the clients,
who would most likely be checking for incoming mail anyway at the
time when they are sending mail out. It does, however, require you
to be aware that the POP login is necessary.

:p.When you enable the "POP before SMTP" option, a new dialogue
is shown where you have to enter four details: the username and
password for one of your POP accounts; the POP port (normally
110); and the hostname of the machine on which the POP server is running.
(This might be the same as the SMTP server hostname, but sometimes
it is different.) Major Major will log into that account, and
then log out without fetching any mail, before sending any outbound
mail.

:p.:hp2.Changing the authentication details:ehp2.

:p.The "Outgoing mail authentication" dialogue appears when you
check the "use SMTP authentication" checkbox on the Basic page of
the Admin notebook.
If you later want to change these details, click on the "use SMTP
authentication" checkbox twice: once to disable it, and once more to
enable it again.

.***********************************
.*   FILES
.***********************************

:h2 id=Files.Files
:hp2.Files:ehp2.

:p.The first item on this page is about logging the Major Major operations.
You can send the log to disk, or to the screen, or both (or neither).
Note that logging to the screen will not work if you run Major Major as
a detached program.

:p.If you enable logging to disk, you need to give the name of the
log file. The default choice is MAJOR.LOG, in the current working
directory. You can put the log into a different directory, if you
wish, simply by specifying a file name that includes either a relative
or an absolute path specification. (Example: E&colon.\LOGS\MAJOR.LOG)

:p.You can also specify a directory where list archives are to be
kept. Again, this can be a simple name relative to the current
working directory, or it can be a path name for a directory that
is located somewhere else. Each list then keeps its archives in a
subdirectory of that directory. Of course, the archive directory
specification is relevant only if you enable archiving for one or
more lists.

:p.If you want to enable archiving for a list, look in the
:link reftype=hd refid=ListArchive.Archive page:elink. for that list.


.***********************************
.*   ADMIN
.***********************************

:h2 id=Admin.Admin
:hp2.Admin&colon. settings for the Major Major administrator:ehp2.

:p.The administrator is the part of Major Major that receives and processes commands
such as "subscribe". Changes made on this page will not have any effect until you restart
Major Major.

:p.:hp2.E-mail account for the administrator:ehp2.

:p.Here you specify the username, the login name, and the POP3 password for the e-mail account that
receives mail for the administrator. (Note: this refers to the "administrator"
component of the Major Major software, not to a human administrator.)
Enter the details here as specified by
your postmaster. These fields might or might not be case-sensitive, depending
on the mail server. With most mail servers the login name is exactly the same
as the username, but some servers make a distinction between the two. The
username is the part before the '@' in the e-mail address, and the login name is the
name that has to be given when logging in to fetch mail.

:p.The default assumption is that the username is "MajorMajor", but you can
choose some different name if you prefer and/or if your postmaster requires it.

:ul.
:li.Special case 1&colon. If you have selected "Weasel on this machine" as your mail
server, the login name and password are not needed.
:li.Special case 2&colon. Some multi-domain POP servers require you to log in with
a string of the form "username@domain" as the username. If so, you should
include this complete string as the login name.
:eul.

:p.:hp2.Response to faulty input:ehp2.

:p.Normally the mail to the list administrator will consist of commands like
"subscribe listname", and a few other possibilities. If spammers find the
e-mail address of the list administrator, they will of course send something
totally different. If you specify "ignore" for this option, the spam will be
ignored.

:p.Note, however, that many genuine users are not perfect typists, and they will
sometimes send invalid commands. The "ignore" option will also reject typing
errors. It will also ignore HTML mail. You will need to use your own judgement
here. You might well choose to "accept" faulty input, if you happen to host
mailing lists whose subscribers include bad typists or people who do not
know how to send "plain text" e-mail.

:p.:hp2.Filter:ehp2.

:p.You can use this option if you have written a filter that checks for legitimate
mail to the Major Major administrator mail address. If you use this option, you
should enter the name of a program (an executable file, a Rexx script, a Perl
script, etc.) that should be called each time mail to the administrator address
is received.

:p.The filter is given one parameter, which is the filename of the file that
contains the incoming message.

:p.The filter should return one of two possible results:
:ul compact.
:li.0       if the mail should be accepted
:li.1       if the mail should be rejected
:eul.

:p.
:p.:hp2.Time between checks:ehp2.

:p.This specifies how often the Major Major administrator will check for new
mail for itself. (This has nothing to do with the times for checking the
mailing lists. The time is set independently for each list.) Typically
this is not urgent mail, so you don't need frequent checks.

:p.:hp2.Admin language:ehp2.

:p.This is a code for the language to be used for administrative functions
such as writing to the log file. It is customary to use two-letter codes, but this field
permits you to go as high as 32 characters if you wish.

:p.If the language code is xy, the messages are taken from a file called
MM.xy.lng. If this file does not exist, the file MM.en.lng is used. If you wish
to create a language file for another language, take a copy of MM.en.lng,
rename it, and replace the messages inside the quotation marks in the
obvious way. If you want your translation to be distributed with Major Major,
put your name in the comments at the top of the file so that you can get
credit for your work.

:p.The admin language is also used for the labels in the Admin notebook
itself. If the language code is xy, the notebook labels are taken from
the file called Admin.xy.lng. If this file is missing, the labels will
be in English. (Unless Admin.en.lng is also missing, in which case the
labels might be blank.)

:p.:hp2.Default language:ehp2.

:p.This also is a code to specify a language, which might or might not be
the same as the Admin language. This is the language that is used in
replies from Major Major when no other language has been specified.

:p.Note that you can specify a different language to associate with
:link reftype=hd refid=ListBasic.each list:elink.. This is initially the default
language, but of course you can change this in the list properties.

:p.:hp2.MIME character set:ehp2.

:p.This should be the MIME code for the character set to be used in
e-mail responses to commands sent to Major Major. The default value is
iso-8859-1, which is a good choice for many Western European languages.
You should probably set it to the MIME code for the character set most
commonly used for the default language as specified above. This is a
compromise, because it is impossible to allow for all possible languages,
and the best you can do is to choose a character set that contains as
many characters as possible from those used in the messages sent back
to users.

:p.(In fact, we could allow for all languages if we used Unicode, and
that is what I would hope to use in some future version of the software.
For now it is not an acceptable choice unless you are certain that all
subscribers to your list use e-mail software that can correctly decode
Unicode. Unfortunately, some of the most popular mail clients for OS/2
do not understand Unicode. Indeed, some of them claim to understand Unicode,
but they still use "?" characters to indicate character codes that they
don't understand.)

.***********************************
.*   MESSAGES
.***********************************

:h2 id=Messages.Messages
:hp2.Messages&colon. names of files containing some standard messages:ehp2.

:p.Each entry on this page contains the name of a file. The file contains
a message that will be e-mailed as a reply to certain operations. You
can choose to use the default messages in the "Canned" subdirectory, or
you can write your own versions. If you write your own, give them file
names or directories that are different from the default file names;
otherwise, you risk having your modifications overwritten when you install
a new version of Major Major.

:p.For each of these messages, the filename may contain the substring
"%L" (without the quotation marks). If it does, the %L is replaced by
the character code for the language that is being used to send back
messages to the user. This allows you to have different versions of the
message for each supported language. If the macro expansion produces
the name of a file that does not exist, the corresponding file for
the default language is used instead.

:p.:hp2.Message when non-subscriber attempts to post:ehp2.

:p.This file should contain the reply that will be sent to someone who
attempts to send a message to a list that they are not allowed to
write to. The first line is normally a "Subject:" line. After this
you can add more header lines if you wish. (You can, for example, add
some MIME header lines if you want to use a character set that is
different from US-ASCII. This is a common requirement if your message
is not in the English language.) After the header lines there
should be a blank line, and then the body of the message.

:p.:hp2.Response to HELP command:ehp2.

:p.This file should contain the text that will be sent back to a user
in response to a HELP command. There are no header lines.

:p.:hp2.Message when the mail to Major Major is not in 'plain text' format:ehp2.

:p.It is becoming increasingly common for users to send mail that
contains HTML markup, and the end result of this is a sequence of
commands that Major Major cannot understand. (We could probably solve
this by adding some sort of HTML stripper, but at least for now I
consider such solutions to be a waste of resources. It is unreasonable
to have to add complicated processing algorithms whose only purpose
is to compensate for the faults of someone's badly configured mail
program.) Instead of giving cryptic error messages, Major Major
attempts to send a more meaningful error reply to anyone whose
commands are not in 'plain text' format. The default reply in this
case can be found in the 'Canned' subdirectory, but you can replace
this with a different file if you prefer. There are no header lines.

:p.See also the section on
:link reftype=hd refid=ListMessages.macros:elink..

:note.The restriction to plain text applies only to messages sent
to Major Major itself, such as the "subscribe" command. HTML messages
to the mailing lists are perfectly legal.

.***********************************
.*   LISTS
.***********************************

:h2 id=Lists.Lists
:hp2.Lists&colon. creation and deletion of mailing lists:ehp2.

:p.The listbox on this page lists all the mailing lists that are currently
defined. Changes made on this page will take effect immediately; it will
not be necessary to restart Major Major.

:p.:hp2.Adding a new mailing list:ehp2.

:p.To create a new list, click on the "Add" button. You will be asked
to enter a name for the list. Type in the name, finishing with the
<Enter> key. You may then proceed to
:link reftype=hd refid=EditList.edit the list properties:elink..

:p.:hp2.Editing the properties of an existing mailing list:ehp2.

:p.To edit a list, first select that list (e.g. by clicking on its
name with the left mouse button), and then click on the "Edit" button.
Then proceed as described in the section on
:link reftype=hd refid=EditList.editing the list properties:elink..

:p.Instead of selecting the "Edit" button, you may also double-click
on the list name.

:p.:hp2.Changing the name of an existing mailing list:ehp2.

:p.To rename a list, first select that list (e.g. by clicking on its
name with the left mouse button), and then click on the "Rename" button.
You will then see a small entry box where you can edit the name.
Type the <Enter> key when you have finished. This operation changes
only the name of the list, and therefore its e-mail address. All other
properties of the list are kept unchanged.

:p.:hp2.Deleting an existing mailing list:ehp2.

:p.To delete a list, first select that list (e.g. by clicking on its
name with the left mouse button), and then click on the "Delete" button.

:p.Warning: there is no way to undo this operation. As a safety measure,
it is a good idea to use the
:link reftype=hd refid=DumpINILoadINI.DumpINI:elink. utility to create a
backup copy of your data.

.***********************************
.*   ABOUT
.***********************************

:h2 id=About.About
:hp2.About:ehp2.

:p.The function of the "About" notebook page is to identify the author, and to tell you
what version of the program you have.

.***********************************
.*   REMOTE CONFIGURATION
.***********************************

:h1 id=remoteconfig.Remote configuration

:p.Admin also offers the option of remote mailing list administration. That
is, you can run Admin on one computer and use it to configure a copy
of Major Major that is installed on a different computer. To do this, you have
to have the freeware utility INIServe running on the same computer as
Major Major. You can find INIServe at http&colon.&slash.&slash.www.pmoylan.org&slash..

:p.If you select the "Remote" radio button after starting Admin, a "Setup"
pushbutton is enabled. Clicking on this gives you four fields to fill in&colon.

:dl break=all.
:dt.     Hostname
:dd.The name (or IP address) of the machine on which Major Major is running.
:dt.     INIServe port
:dd.The TCP port that INIServe has been configured to listen on. The default
value is 3560.
:dt.     INIServe password
:dd.The password needed to log in to your copy of INIServe.
:dt.     Major Major directory
:dd.The full path name of the directory, on the remote machine, where Major Major
is installed.
:edl.

:p.When you close the Setup window, you can click on the "GO" button to connect
to the remote machine. If this gives a "failed to connect" or similar error
message, it probably means that you don't have INIServe running on the
remote machine, or that you have done something like specifying an incorrect
port number.

:p.Once the connection is made, the operation is the same as for the
case of local configuration.

.***********************************
.*   SUPPORTING MULTIPLE DOMAINS
.***********************************

:h1 id=multidomain.Supporting multiple domains

:hp2.Supporting multiple domains:ehp2.

:p.If your computer supports more than one domain, or you have ISP accounts for
more than one domain, you might want to have mailing lists in more than one
domain.

:p.Major Major does not have a multidomain mode, but this is not a barrier.
All you need to do is run several instances of Major Major, one for each domain.
(You don't need several copies of Major.exe to do this; just several program
objects, typically in different directories, that all point to the same Major.exe.)
The following description shows one way of doing it, for two domains called
domain1 and domain2. Variations
on this approach will be obvious once you see the basic idea.

:ol.
:li.Make sure you have a Major Major mail account for each domain. That is,
you need mail accounts majormajor@domain1 and majormajor@domain2.
:li.Configure the first domain as described in the
:link reftype=hd refid=Configuration.configuration:elink. section.
:li.Create a subdirectory domain2 of the main Major Major directory.
Copy your MAJOR.INI into this subdirectory.
:li.Create a program object for MAJOR.EXE, and put this program object in
the domain2 subdirectory. Give it a distinctive name, for example Major.domain2.
Open the Properties notebook for this program
object. The "Path and file name" field should already point to the
original MAJOR.EXE, but you need to modify the "Working directory" to
specify the domain2 subdirectory.
:li.Create a program object for ADMIN.EXE in the same way.
:li.Open the new admin.domain2 program object. Modify the Basic page to
specify domain2 as the Mail domain name. On the Lists page create the lists
for domain2, and make whatever other customisations you want. On the Messages
page, and also on the Messages1 and Messages2 pages for each list, you will
probably have to specify a "..\" at the beginning of each file name.
:eol.

:p.Now you have two instances of Major Major, one for each domain.

:p.Note that you still only have one copy of each of MAJOR.EXE and ADMIN.EXE.
If each instance has a different working directory, and therefore a different
place to put its INI file, that is sufficient to make the instances independent
of each other.

.***************************************
.*   EDITING THE MAILING LIST PROPERTIES
.***************************************

:h1 id=EditList.Defining the properties of a mailing list
:hp2.Defining the properties of a mailing list:ehp2.

:p.You reach this point by going to the "Lists" page in the Admin
notebook and choosing the "Add" or "Edit" option. This opens up a
new notebook with the list properties.

:p.The book has the following pages.

:ul compact.
:li.:link reftype=hd refid=ListBasic.Basic settings:elink.
:li.:link reftype=hd refid=ListOptions1.Options 1:elink.
:li.:link reftype=hd refid=ListOptions2.Options 2:elink.
:li.:link reftype=hd refid=ListMembers.Members:elink.
:li.:link reftype=hd refid=ListOwners.Owners:elink.
:li.:link reftype=hd refid=ListMessages.Messages:elink. (2 pages)
:li.:link reftype=hd refid=ListArchive.Archive:elink.
:eul.

:p.Any changes you make will come into effect once you close the list notebook.
It is not necessary to restart Major Major.

.***************************************
.*   LIST NOTEBOOK: BASIC PAGE
.***************************************

:h2 id=ListBasic.Basic settings
:hp2.Basic properties of the list:ehp2.

:p.The settings on this page define some basic properties of the list.

:dl break=all.
:dt.POP3 login name
:dd.This is the username that is needed to log in to the POP3 server
when fetching mail for this list. With most POP3 servers this will be
exactly the same as the list name, but this option lets you
work with mail servers where the 'login name' is not the same as the
username used in the e-mail address.
:dt.POP3 password
:dd.This is the password that is needed to log in to the POP3 server and
fetch mail. It will have been assigned to you by the person in charge
of the mail server. In the special case where you have specified
"Weasel on this machine" as your mail server, the POP3 login name and
password are not needed, so these two entries are ignored.
:dt.Abbreviated list name
:dd.Here you can specify an abbreviation for the list name, up to
32 characters long. This will appear in brackets at the beginning
of the "Subject:" line when mailings are distributed to the list
subscribers.
:dt.Check for new mail
:dd.Specify here how often the POP3 server is to be checked for new
mail to this list. Making this time too short can put an unreasonable
load on your processor. A time in the range 1 to 10 minutes is a
reasonable compromise. If list messages are not particularly urgent
you could even reduce this to once per day.
:p.If you have chosen "Weasel on this machine" as your mail server,
you can set this to a long time, because there will be in any case
an automatic check each time Weasel receives new mail.
:dt.Error messages
:dd.Error messages will be produced by the receiving SMTP servers when,
for example, somebody subscribes to the list with an invalid address.
You can specify "discard" to say that these messages should be ignored,
or you can specify an e-mail address to receive such messages. The
address will typically be the same as the one of the list owner
addresses on the "Owners" page, but does not need to be. The address
specified here will be the used in the MAIL FROM: command that is used
when sending mail for this list to recipients, and therefore it is
also the address seen by recipients in the "Return-Path:"
header line. :hp2.Note:ehp2.. Some mail servers might reject mail from
Major Major if you don't supply an address here.
:dt.Filter
:dd.The entry in the "Filter" field, if present, should be the name
of an executable program or of a script (in a language like Rexx
or Perl) that will filter the
incoming messages. The filter takes one parameter, which is the
name of a file that contains the incoming message. The filter may
modify the message, but it should not change the file name. If the
reply code from the filter is zero, the (possibly modified) message
will be delivered to all subscribers to the list. If the reply
code is nonzero, the message will be deleted and not delivered.
:dt.Language
:dd.This is a code for the language to be used when Major Major sends
messages to the users. If the language code is xy, the messages are
taken from a file called MM.xy.lng. Note that there is one default
language specified on the "Admin" page of the main notebook, and this
is used for messages that are not specific to any list. The present
entry is used for messages that belong to this list.
:edl.

.***************************************
.*   LIST NOTEBOOK: OPTIONS 1
.***************************************

:h2 id=ListOptions1.Options 1
:hp2.Option settings for this list:ehp2.

:p.:hp2.Remove attachments:ehp2.

:p.
If this option is checked, all attachments are stripped from messages
sent to the mailing list, leaving only the text part. This is a
desirable option because mail attachments are a common way of
distributing viruses. In addition, even non-virus attachments are
considered by many people to be a nuisance, because they add to the
size of the message and the time taken to receive it.

:p.:hp2.Moderated list:ehp2.

:p.
If you tick the "List is moderated" box, this list becomes a
:link reftype=hd refid=Moderation.moderated:elink.
list. In a moderated list only the list owners may
post messages to the list. (You should make sure that the "Owners" page
for this list contains at least one entry.)
If anyone else sends mail to a moderated list, that mail will be sent
to the list owners rather than to the whole list. The copy received
by the owners will have an extra header line "X-For-Moderation:"
that tells the owners which list this message belongs to.
It is then up to
the owners to decide whether to re-post the message to the list.
The "X-For-Moderation:" header line is removed by Major Major when the moderator
reposts the message, i.e. that header line is seen only by moderators
and not by anyone else.

:p.:hp2.Suppress sender address:ehp2.

:p.
If you enable this option, mail sent to the list will have the "From"
address removed so that list members will not be able to tell who
wrote the message. This is appropriate in the sort of list that
discusses matters that are so confidential that list members need
to be confident that their contributions are anonymous.

:p.On the negative side, this feature also allows troublemakers to
be anonymous. List owners will have to make their own decision as
to whether anonymity is desirable.

:p.:hp2.DMARC compatible:ehp2.

:p.DMARC is a set of rules that are intended to block spam. It is not
yet clear whether it will be widely adopted, but it is being adopted
by those organisations like Yahoo and AOL that give out e-mail
addresses both to spammers and to genuine users. It does not stop
spam from going out from those providers, but it is supposed to help
prevent some spam from being received by them. Unfortunately, it also stops mail from
genuine mailing lists, because it detects that the listserver is
sending out mail from list members whose mail accounts are not in the
same domain as the listserver.

:p.It has been suggested that the best solution to this problem is to
blacklist the spammer-friendly providers, or at least not allow
people with mail accounts with those providers to become list members.
Sometimes, however, you might have no choice. Enabling this option
changes the "From:" header to get around the DMARC restrictions, and
makes some associated header changes.

:p.The price for DMARC compatibility is that it puts you in technical
violation of the standards governing mail headers. Thus, you should not
enable this option unless you really need to.

:p.:hp2.Controlled commands:ehp2.

:p.Certain commands that may be sent to Major Major are
"controlled" in the sense that you decide, by checking the
appropriate options on this page, whether the commands will be
considered legal. The list owners are always allowed to send
controlled commands, regardless of the settings on this page.
Other people can use a controlled command only if it has been
enabled by a check-mark on this page for that command.

:p.In the present version of Major Major, the following commands
are controlled&colon.

:dl break=all.
:dt.    subscribe
:dd.This is the basic "please subscribe me to this list" command.
If you do not enable this command, nobody except the list owners
may subscribe by sending an e-mail to Major Major. Everyone
else has to be subscribed by the list owners, or manually.
:dt.    subscribe, long form
:dd.This refers to subscription requests of the form
:xmp.

      subscribe listname emailaddress:exmp.
:p.where the person being subscribed is not necessarily the person
who has sent the command. If you disable this command then the
list owners can subscribe other people, but nobody else can.
:dt.    unsubscribe, long form
:dd.This is similar to the previous case, but for unsubscribing.
:dt.    who
:dd.The "who" command allows anyone to find out who is already
subscribed to the list. You might or might not want to enable
this, depending on whether you consider the list of subscribers
to be confidential.
:edl.

:p.Note that the short form of "unsubscribe" cannot be
disabled. Major Major is not intended to be used for sending
junk mail, therefore it is always possible for recipients to
remove themselves from the mailing lists.

:p.:hp2.Default character set for archives:ehp2.

:p.The value of this field is a string, which must be a character set
code that may legally be given as the 'charset=' parameter
in the 'Content-type&colon.' header line of an e-mail message. Major Major
does not check the value for legality, because the set of legal
values can change as the standards are revised. The use of an
illegal value can, however, confuse the mail readers of the recipients
of the mail. The default value is 'iso-8859-1', which is the character
set most commonly used for many western European languages. (Which makes
it a good choice for English speakers, among others.) Another
popular choice is 'iso-8859-15', which has the advantage of supporting
the Euro currency symbol, but that one is not much used by OS/2 users
because it is not, unfortunately, supported by some popular
OS/2 e-mail readers. If you leave this field completely blank,
Major Major will assume that you want the 7-bit us-ascii code.

:p.This option is relevant mainly for archives, and therefore for
digests. For "normal" list mail, each message carries its own mail
headers that specify, among other things, the character set used
by the sender. (At the receiving end, most mail programs are smart
enough to translate from that character set to the one used by the
person reading the mail. Of course, there is always a limit to how many
different character sets the mail program will support.) For a
digest message, on the other hand, there is one single character
set encoding for the entire message. That means that you need to
specify a compromise character set, the one most likely to be used
by most list members. For a multilingual mailing list, you might
have to insist that everyone use unicode.

:p.:hp2.RFC 2919 List ID:ehp2.

:p.You can safely leave this field blank. If it is not blank, list
messages will have an extra header line of the form
:xmp.

      List-Id: <0Jks9449.list-id.cmu.edu>
:exmp.
:p.or
:xmp.

      List-Id: <da39efc25c530ad145d41b86f7420c3b.052000.localhost>
:exmp.

:p.where the part inside the <> delimiters is the ID that you have
entered in this field. The motivation for having such a header
line is that it provides a persistent identifier for the list
that remains valid even if the list is moved to a different
server. (The e-mail address of the list is also a persistent
identifier, in most cases, but it will change if you ever have
to move your lists to a different mail server.) Subscribers to
the list can therefore use it to identify which of their incoming
mail items is a message belonging to this list.

:p.There are rules for constructing a valid list ID, and these
are described in the draft standard RFC2919. The examples above
are taken from that document. (If you do not know
what an RFC is, do a web search for RFC2919. Numerous HTTP and FTP
servers all over the world keep copies of the RFCs, so you should
have no trouble finding a suitable server near you.) Basically,
the ID should have the appearance of a hostname in a domain that
belongs to you. If you do not own a domain name you can use the
domain name "localhost", but then you should follow the recommendations
of RFC2919 for generating a suitably randomized ID, to minimise the
risk of choosing an ID that duplicates one that someone else has
chosen.

:p.:hp2.RFC 2369 headers:ehp2.

:p.RFC 2369 is another draft standard specifying mail header fields
for use by mailing lists. These give information on things like
how to get help, how to unsubscribe, and similar details. The easiest
way to see how this works is to enable this option and look at the
resulting header lines in mail for the list. Most mail programs
simply ignore this information, but some - Pine, for example - use
these fields to distinguish between mailing list mail and ordinary
mail. Enabling this option tells Major Major to include the extra
header lines in all outgoing mail for this list.

.***************************************
.*   LIST NOTEBOOK: OPTIONS 2
.***************************************

:h2 id=ListOptions2.Options 2
:hp2.More options:ehp2.

:p.:hp2.Require confirmations:ehp2.

:p.If you enable the 'require confirmations' option, this changes the way
that the 'subscribe' and 'unsubscribe' commands work. When someone tries to
subscribe to this list, or unsubscribe from it, the command does not take
effect immediately. Instead, an e-mail is sent to the user asking for
the command to be confirmed. The e-mail contains a pseudo-random confirmation
code, which must be used by the user when replying to the confirmation
request. If the user responds with an incorrect code, or if the user does
not respond at all, then the subscription or unsubscription will not happen.

:p.This option is needed if there is a risk of fake subscriptions. If
someone tries to subscribe to the list using a fake e-mail address, the
true owner of that address will receive the confirmation request. In
effect, the confirmation request message is asking 'did this request really
come from you?' It is a guard against people being subscribed to a mailing
list without their knowledge.

:p.The confirmation option includes a time limit. If you enable this option,
you must specify the time (in hours) by which the confirmation must be
received. For a genuine request, the confirmation will probably be returned
within a few minutes. If you wish to allow for more extreme possibilities,
such as a subscription request on a Friday with the confirmation on the
following Monday, you can increase the time allowed.

:p.The text of the message that the intending subscriber will receive is
specified on the
:link reftype=hd refid=ListMessages.List Messages:elink. page of this notebook.
The subject line of this message must contain the %X macro, to ensure that
the confirmation code is sent to the user.

:p.:hp2.Owners may confirm 'unsubscribe':ehp2.

:p.If you enable this option, and confirmations are also enabled, then
a copy of the confirmation message for any 'unsubscribe' operation is
sent to the list owners as well as to the person unsubscribing. This
allows the owners to confirm the operation even if there is no
answer from the list member. We need this option to handle the case
where the mailing list contains an obsolete e-mail address, so that there
is nobody at the receiving end to do the confirmation.

:p.:hp2.Mail from non-subscribers:ehp2.

:p.Here you have to decide whether people not subscribed to the list
should be able to post messages to the list. Choose "Accept" if you
want to permit this. Choose "Reject" if you want to refuse the message
and send an error message back to the sender. Most commonly you will
want to choose "Ignore", which is like "Reject" except that no error
message is sent back. This is because a lot of junk mail software
takes a reply as a confirmation that this e-mail account is active
and able to receive more junk mail.

:p.The next option on this page is to say whether you want to keep
a copy of refused messages. (A message for this mailing list is defined
to be 'refused' if it is ignored, or rejected, or rejected by a
:link reftype=hd refid=ListBasic.filter:elink..) Normally you would
:hp1.not :ehp1. want to keep these messages, because the whole point
of controlling access to a mailing list is to keep out the junk mail and
the mail from malicious users. You might, however, need to keep a copy
of such messages if you are trying to track down the origin of a
problem. If you select the "saved refused mail" option, you have to
specify a directory where that mail will be stored. If you leave the
directory name blank, those messages will be saved in the Major Major
directory itself, which is probably not what you want. The name may
either be a full pathname, or a name relative to the directory from
which Major Major is running.

.***************************************
.*   LIST NOTEBOOK: MEMBERS PAGE
.***************************************

:h2 id=ListMembers.Members
:hp2.Subscribers to this list:ehp2.

:p.Normally the subscribe/unsubscribe operations are handled by the
list members themselves. They send an e-mail to put themselves on
the mailing list or to remove themselves from the list. There might,
however, be times when you want to make changes manually.

:p.:hp2.Adding a new member:ehp2.

:p.Click on the "Add" button, and type in an e-mail address. Finish with
the Enter key to confirm your entry, or with the Esc key to cancel the
operation. It is legal to specify
:link reftype=hd refid=Membershipflags.membership flags:elink.
in front of the e-mail address.

:p.:hp2.Changing an existing entry:ehp2.

:p.Click on the entry to be changed, click on the "Revise" button, and edit the
e-mail address. Finish with
the Enter key to confirm your entry, or with the Esc key to leave the
original entry unchanged. It is legal to put
:link reftype=hd refid=Membershipflags.membership flags:elink.
in front of the e-mail address.

:p.:hp2.Deleting an existing member:ehp2.

:p.Click on the entry to be deleted, then click on the "Delete" button.

:p.Double-clicking on a list entry has the same effect as selecting the
"Revise" button.

.***************************************
.*   MEMBERSHIP FLAGS
.***************************************

:h3 id=Membershipflags.Membership flags
:hp2.Membership flags:ehp2.

:p.A "normal" list member is able to both send and receive list messages,
but there are some special cases where we want to alter this behaviour.
To allow for the special cases, we allow for three special attributes
for a list member.

:dl break=all.
:dt.    readonly
:dd.A readonly member receives the list messages, but cannot send to
the list.
:dt.    writeonly
:dd.A writeonly member can send messages to the list, but does not
receive any messages from the list.
:dt.    digest
:dd.Digest members are explained on the
:link reftype=hd refid=ListArchive.Archives and digests:elink. page.
:edl.

:p.On the "Members" notebook page, these are shown as one-letter
codes in parentheses before the e-mail address, for example
:xmp.

           (R)someone@somewhere.com

:exmp.
The possible codes are

:dl break=none.
:dt.    (-)
:dd.This indicates a member who can neither send to nor receive
messages from the list. In effect, this is someone who has been
removed from the list, but whose address has been left in the list
for possible future activation. You might, for example, use this
code for someone who is on vacation.
:dt.    (R)
:dd.A readonly member.
:dt.    (W)
:dd.A writeonly member.
:dt.    (D)
:dd.A digest member.
:dt.    (RD)
:dd.A readonly digest member. This means a digest subscriber who
receives the list digest, but who may not post to the list.
:edl.

:p.Note that there is only one case, namely (RD), where the attributes
may be combined. All other combinations are meaningless, and will
produce unpredictable results.

:p.The 'writeonly' attribute is unusual, and potentially dangerous,
so ordinary list members are not permitted to subscribe with this
attribute. The only ways to subscribe someone as a writeonly member
are to edit the entry directly in the Admin notebook, or via a
'subscribe' command from an owner of the list. The reason for allowing
the 'writeonly' option is to allow for special cases such as lists
which are echoes of other lists.

.***************************************
.*   LIST NOTEBOOK: OWNERS PAGE
.***************************************

:h2 id=ListOwners.Owners
:hp2.The owners of the list:ehp2.

:p.The owners of the list, who might or might not also be members
of the list, have two special functions.

:ol.
:li.They are allowed to use the :link reftype=hd refid=ListOptions1.controlled commands:elink.,
even those that are disabled for ordinary users.
:li.If the list is a moderated list, the list owners are the
:link reftype=hd refid=Moderation.moderators:elink..
They receive the mail submitted to the list, and decide
whether it should be sent out again.
:eol.

:p.If you don't need either of these features, then you can leave the
"Owners" list empty. For a moderated list, it is of course essential
to have at least one owner. In most cases a single owner (moderator)
is sufficient, but we allow for multiple owners to handle lists where
the job of moderating must be shared among several people.

:p.:hp2.Adding another owner:ehp2.

:p.Click on the "Add" button, and type in an e-mail address. Finish with
the Enter key to confirm your entry, or with the Esc key to cancel the
operation.

:p.:hp2.Changing an existing entry:ehp2.

:p.Click on the entry to be changed, click on the "Revise" button, and edit the
e-mail address. Finish with
the Enter key to confirm your entry, or with the Esc key to leave the
original entry unchanged.

:p.:hp2.Deleting an existing owner:ehp2.

:p.Click on the entry to be deleted, then click on the "Delete" button.

:p.Double-clicking on a list entry has the same effect as selecting the
"Revise" button.

.***************************************
.*   LIST NOTEBOOK: MODERATION
.***************************************

:h3 id=Moderation.Moderation
:hp2.Moderation:ehp2.

:p.In a moderated list, messages posted to the list are not immediately
distributed. Instead, they are sent to the list owners for approval. Any
list owner may approve the message by re-posting it to the list. (There is,
at present, no check for multiple re-postings, so it is possible for a
message to be posted more than once if more than one list owner approves
it. We consider this to be such a rare situation that there is no real
need to guard against it.) Typically a list will have only one owner, and
therefore only one moderator, but it is certainly possible to nominate
more than one owner.

For the moderator's benefit, list messages passed on in this way will
have two extra header lines:

:ul compact.
:li.X-For-Moderation: list name
:li.X-Original-Sender: the e-mail address of the sender of the message
:eul.

:p.This tells the moderator that this is a list message that is waiting to
be approved. If the moderator approves the message, it can be forwarded
back to the list to be distributed.

:p.If the mail software used by the moderator retains these headings,
Major Major will use the X-Original-Sender heading field to re-create
the "From:" header in the posted message. Unfortunately some popular
mail programs, including Thunderbird, delete the X-Original-Sender header
line when forwarding a message. For now we have no solution for this
problem, so moderated messages will always appear to have the moderator
as the original poster.

.***************************************
.*   LIST NOTEBOOK: MESSAGES
.***************************************

:h2 id=ListMessages.Messages
:hp2.Messages for this list:ehp2.

:p.These two pages are for specifying the file names of message files for
use with this list. You
can choose to use the default messages in the "Canned" subdirectory, or
you can write your own versions. If you write your own, give them file
names or directories that are different from the default file names;
otherwise, you risk having your modifications overwritten when you install
a new version of Major Major.

:p.All messages are optional, except when they are obviously needed.
(For example, a confirmation request message must be available if
you have specified that this list requires confirmations.) Leave the file name
blank if you do not want to use the option.
The actual messages are listed on the following two pages&colon.
:ul compact.
:li.:link reftype=hd refid=ListMessages1.Messages 1:elink.
:li.:link reftype=hd refid=ListMessages2.Messages 2:elink.
:eul.

:p.:hp2.The %L macro in a message filename:ehp2.

:p.For each of these messages, the filename may contain the substring
"%L" (without the quotation marks). If it does, the %L is replaced by
the code for the language that is being used to send back
messages to the user. This allows you to have different versions of the
message for each supported language. If the macro expansion produces
the name of a file that does not exist, the corresponding file for
the default language is used instead.

:p.Note: inside a message file %L has a different meaning - see below.

:p.:hp2.Macros:ehp2.

:p.In these messages the '%' character has a special function.
This plus the following character forms a two-character macro that
causes special values to be inserted into your text. The macros
currently available are&colon.

:dl break=none.
:dt.    %A
:dd.the e-mail address of the list administrator.
:dt.    %D
:dd.the domain name of our server.
:dt.    %H
:dd.the time, in hours, allowed for subscribe/unsubscribe confirmations
for this mailing list. Obviously this value is meaningful only for those
lists that require confirmation.
:dt.    %L
:dd.the name of this mailing list.
:dt.    %S
:dd.the e-mail address of the sender of this message. Exception: in a
subscribe/unsubscribe confirmation message, %S expands to the e-mail
address of the person being subscribed or unsubscribed.
:dt.    %X
:dd.an extra parameter whose meaning depends on the particular message
that is being sent. In the present version, this is used only in the
confirmation request that is sent to a user who is attempting to
subscribe or unsubscribe (for a list where confirmations are compulsory).
:dt.    %%
:dd.the '%' character itself.
:edl.

:p.

.***************************************
.*   LIST NOTEBOOK: MESSAGES 1
.***************************************

:h3 id=ListMessages1.Messages 1
:hp2.Messages for this list:ehp2.

:dl break=all.
:dt.Message to owners after 'subscribe'
:dd.This, if present, is the name of a file that contains a
message that will be sent to the list owners (if any) each time
someone successfully subscribes to the list. Use this option if
the list owners want to know who is subscribing to the list.
Leave the file name blank if the owners do not want to receive
such messages.
:dt.Message to owners after 'unsubscribe'
:dd.As above, except that this message is sent to
the list owners each time
someone successfully unsubscribes from the list.
:dt.Confirmation request message
:dd.This is the name of a file that contains a message that will be
sent in response to a 'subscribe' or 'unsubscribe' command, for a
list where confirmations are compulsory. (See the
:link reftype=hd refid=ListOptions2.Options 2:elink. page for an
explanation of confirmations.) The message must contain a header line
:xmp.
     Subject: %X
:exmp.
because this is the method used to tell the user what the confirmation
code is. You may, if you wish, add other header lines to specify things
like the MIME character set.
:edl.

.***************************************
.*   LIST NOTEBOOK: MESSAGES 2
.***************************************

:h3 id=ListMessages2.Messages 2
:hp2.More messages for this list:ehp2.

:dl break=all.
:dt.Welcome message
:dd.This is the name of a file that contains a message that will be
sent to all new subscribers to this list. (That is, it will be sent
whenever a 'subscribe' command is successful.) The message should
contain a "Subject:" line, plus any other optional header lines
you want to add. Then there should be a blank line, followed by the
text of the welcome message.
:dt.Departure message
:dd.If non-blank, this is the name of a file that contains a message that will be
sent to a list member who leaves the list by using an 'unsubscribe'
command. It has the same format as the welcome message. The default is
to have no departure message, since we would normally assume that
someone who unsubscribes does not want to receive any more messages,
but some list owners might like to use a 'goodbye' message.
:dt.Leader file
:dd.This file contains text that will be inserted at the beginning
of the body of all list
messages as they are sent out. Most mailing list managers won't
need to use this feature, but it can be used if you want all list
messages to start in a distinctive way.
:dt.Footer file
:dd.This file contains text that will be appended to all list
messages as they are sent out. It would normally contain a signature
line, or a message saying how to unsubscribe from the list, or
something similar.
:edl.

:p.It is legal but not compulsory to begin the leader and/or footer file
with MIME headers. This is explained in the
:link reftype=hd refid=MIMEheaders.following section:elink..

.***************************************
.*   LIST NOTEBOOK: MIME HEADERS IN A LEADER OR FOOTER FILE
.***************************************

:h4 id=MIMEheaders.MIME headers in a leader or footer file
:hp2.MIME headers in a leader or footer file:ehp2.

:p.By default, the leader and footer text (if present) is assumed to
be of type text/plain, with no additional parameters, and strictly speaking
this means that only 7-bit US-ASCII characters are allowed in a leader
or footer file. Some mail programs will accept 8-bit characters anyway,
but what the recipient will see is not necessarily what you intended.

:p.To allow for this, leader and footer files are allowed to have
optional MIME headers to specify the character set and possibly other
encoding details. These header lines are not compulsory, but if present
they must conform to the rules in the MIME standard RFC 2045. A typical set
of MIME headers would be
:xmp.
Content-Type: text/plain; charset=iso-8859-1; format=flowed
Content-Transfer-Encoding: 8bit
:exmp.
In most cases you should specify the 8bit transfer encoding, but if you
want to use something like base64 then that is certainly legal. If you
want to include HTML markup in your leader or footer text, a Content-Type
of text/html is also legal.

:p.The MIME headers, if present, must be followed by a blank line before
the text you want to insert. For an example, see the file
Canned\dk\SampleLeaderFile.txt. Not all the "Canned" files have yet been
converted to use MIME headers, but this will happen gradually with
future Major Major releases.

:p.Notice that all legal MIME headers start with the string "Content-".
(Some software tries also to include a MIME-Version header here, but that
is in violation of the standard.) Major Major uses that fact, plus the fact
that the character '&colon.' must appear in any MIME header line, to decide whether
the file contains MIME headers.

:p.Unless you have an editor that is more intelligent than most editors,
there is no easy way to ensure that the character you are typing is the
correct character for the specified character set. The sample files that are
included with Major Major were prepared using the OS/2 code page 850. If
you are using some other code page, you might have to modify the
non-ASCII characters in those files. As always, if you do modify those files
then you should give your copies new names, or put them in new directories,
so that they will not be overwritten by future Major Major releases.

:note.If you add or remove MIME headers from a leader or footer file,
Major Major might not notice the change until it is restarted, or until the
list properties are modified in Admin.exe.

.***************************************
.*   LIST NOTEBOOK: ARCHIVE PAGE
.***************************************

:h2 id=ListArchive.Archives and digests
:hp2.Creating an archive of the list:ehp2.

:p.If you want to keep copies of messages sent to this list, check
the "Create archive for this list" checkbox on the Archive page of
the list notebook. You also have to specify how many days or months must pass
before the current archive file is closed and a new archive file is
started. If, for example, you specify 28 days then each archive file
for this list will contain a four-week collection of messages. (In
addition, the file called 'arc.tmp' is the next partially built
archive, containing those messages that have arrived since the last
archive file was created.)

:p.:hp2.Warning.:ehp2. The time between archiving can be
unpredictable if your files are stored on a FAT volume, because
the FAT file system has no provision for remembering the creation
date of a file. Newer file systems such as HPFS and JFS do not have this
problem.

:p.The archive files are stored in the
:link reftype=hd refid=Repository.file repository:elink.
for this list. The file names are constructed from the date, to
make it easier to keep them in order.

:p.An archive file is just a concatenation of messages that have
been sent to that list, except that some of the "uninteresting"
header lines have been deleted. The file archives\separator
contains some text (usually just a dotted line, or something
similar) to go in between the messages. You may edit that
separator file if you want a different separator.

:p.:hp2.Digest subscribers:ehp2.

:p.If you choose to create an archive for a list, then list members
may choose to subscribe to a digest of that list. (The way to do
this is described in the section on
:link reftype=hd refid=AdminCommands.commands to the list administrator:elink..)
Ordinary non-digest members receive list messages as they arrive.
The digest members do not receive the messages immediately, but instead
receive a whole collection of messages at the time the archive is made.

:p.The mail received by a digest subscriber is precisely a copy of
the file stored as the archive file. If you have not enabled archiving,
the digest subscribers will receive nothing. And, of course, the
frequency of archiving controls how often the digest is distributed.

.***************************************
.*   THE FILE REPOSITORY
.***************************************

:h1 id=Repository.The file repository
:hp2.The file repository:ehp2.

:p.Optionally, a list may have a file repository associated with it.
If such a repository exists, list subscribers may use the 'index'
command to find out what files are in the repository, and the 'get'
command to fetch files from the repository. In the present
implementation, these must be text files.

:p.Physically, the repository is simply a disk directory. To create
a repository, you can create a subdirectory under the directory
where archives are stored. (The location of this directory is
specified in the :link reftype=hd refid=Files.Files:elink. page
of the Admin notebook.) The subdirectory name
should be the same as the list name.

.***************************************
.*   COMMANDS TO THE LIST ADMINISTRATOR
.***************************************

:h1 id=AdminCommands.Commands to the list administrator
:hp2.Commands to the list administrator:ehp2.

:p.List subscribers and potential subscribers interact with Major Major
by sending it an e-mail message. The "Subject:" header of the message
is normally irrelevant (but see the next paragraph). Apart from this special
case, Major Major ignores everything in the header
except the "Return-Path:" and "From:" addresses. The body of the message contains one
or more commands, which are explained on this page. Whitespace (empty
lines or space characters before the commands) is ignored.

:p.Some list processing software expects commands to be in the "Subject:"
header line, therefore some users are likely to think that Major Major
also works that way. To allow for that possibility, Major Major is also
tolerant of commands in the "Subject:" line. If the subject line contains
a valid command, it is executed. If not, the subject line is ignored.
In either case, Major Major then continues looking for commands in the
body of the message.

:p.Note that some commands can be disabled, by removing the
appropriate checkmarks on the :link reftype=hd refid=ListOptions1.Options:elink.
page for a list.

:p.The legal commands are described below.

:dl break=all.
:dt.:xmp.lang name
:exmp.
:dd.By using this command, the user specifies his or her preferred
language for replies to the subsequent commands. The name argument is
a code, usually a two-letter code, for the language. If it does not
correspond to a language supported by the listserver, the default
language is used.

:dt.:xmp.subscribe listname emailaddress
subscribe listname readonly emailaddress
subscribe listname writeonly emailaddress
subscribe listname digest emailaddress
subscribe listname digest readonly emailaddress
:exmp.
:dd.This is how you subscribe someone to a list. The first parameter is
the name of the list, and the final parameter is the e-mail address of
the person to be subscribed. In addition you may specify readonly,
writeonly, and digest options as shown. (But writeonly is illegal unless
you are a list owner.)

:dt.:xmp.subscribe listname
subscribe listname readonly
subscribe listname writeonly
subscribe listname digest
subscribe listname digest readonly
:exmp.
:dd.This is the same as the above, except that the e-mail address of the
new subscriber is taken from the "Return-Path:" line of the request. If
the "Return-Path:" line is missing then the address is instead taken
from the "From:" line of the request.
:note.If you want to change the readonly, writeonly, or digest options,
you must first unsubscribe and then subscribe again with the new
options.
:dt.:xmp.unsubscribe listname emailaddress
:exmp.
:dd.This is how you remove someone from a list. The first parameter is
the name of the list, and the second parameter is the e-mail address to
be removed from the subscriptions.
:dt.:xmp.unsubscribe listname
:exmp.
:dd.This is the same as the above, except that the e-mail address to be
removed is taken from the "Return-Path:" (if present) or the "From:" line of the request.
:dt.:xmp.help
:exmp.
:dd.This command causes a help message to be sent back to the person
sending the mail.
:dt.:xmp.lists
:exmp.
:dd.This returns a list of all mailing lists on this server.
:dt.:xmp.index listname
:exmp.
:dd.This returns a listing of the files held in the file repository
for this list. If the list has no file repository, an empty
listing is returned.
:dt.:xmp.get listname filename
:exmp.
:dd.Returns a copy of the text file 'filename' from the file
repository for this list.
:dt.:xmp.which
:exmp.
:dd.This returns a list of all mailing lists to which the person
sending the request is subscribed.
:dt.:xmp.who listname
:exmp.
:dd.The result of executing this command is that a list of list members
is sent back to the person sending the mail. This command is illegal if
the person making the request is not a member of the list.
:dt.:xmp.end
:exmp.
:dd.Specifies that there are no more commands. This ensures that Major
Major does not try to process things like signature blocks. If the
'end' command is missing, processing stops either at the end of the
e-mail or at a line starting with a '-' character, whichever comes first.
:edl.

.***************************************
.*   THE LANGUAGE USED FOR REPLIES
.***************************************

:h2 id=ReplyLanguage.The language used for replies
:hp2.The language used for replies:ehp2.

:p.When a user sends mail to Major Major, containing commands as described
on the previous page, we have to decide which language to use for replies.
This decision is made as follows&colon.

:ul.
:li.If this user is subscribed to one or more lists, we work out how many
lists use each language, and then choose the language corresponding to
the greatest number of lists. (In the case of a tie, we make an arbitrary
decision.)
:li.If the user is not subscribed to any list, we tentatively choose
the default language. Then, the first time the user gives a command
(such as 'subscribe') that refers to a specific list, we switch to using
the language associated with that list.
:li.The user can explicitly specify a language at any time by using
the 'lang' command.
:eul.

.***************************************
.*   DUMPINI AND LOADINI
.***************************************

:h1 id=DumpINILoadINI.Dealing with TNI files

:p.All of the configuration data for Major Major, including details such
as the list of all e-mail addresses for a mailing list, are kept in a
binary file called MAJOR.INI, or in a plain text file called MAJOR.TNI.
The first two programs described on this page
allow you to convert between the two formats. Even if you have decided
to stay with the INI file, it is still worth creating a TNI file for backup purposes.
This also gives you the possibility of manually editing the data, for
example when trying to create a mailing list from an address book in
your e-mail software.

:p.These utilities are not distributed with Major Major.
They can be obtained by downloading the GenINI package from
http&colon.&slash.&slash.www.pmoylan.org/pages/os2/software.html.

:p.:hp2.DumpINI:ehp2.

:p.The DumpINI program reads MAJOR.INI and creates a new file called
MAJOR.TNI. This new file contains the same data, but in human-readable
form.

:p.:hp2.LoadINI:ehp2.

:p.The LoadINI program reads MAJOR.TNI and loads the information back
into the main INI file called MAJOR.INI. The previous contents of
MAJOR.INI, if any, are lost, but may be found in a backup file.

:p.:hp2.TNItools:ehp2.

:p.The Tools subdirectory of Major Major contains some Rexx scripts
that some users might find useful. Some of those scripts need to
look up information in an INI or TNI file. To support that, the
TNItools package contains some extra Rexx scripts that deal with
reading from or writing to INI and TNI files. These are
prerequisites for some of the Major Major Rexx tools.

:p.TNItools is part of the GenINI package mentioned above.
It is not included with Major Major.

:p.It is recommended that you put the TNItools scripts in a directory
that is on your PATH (as defined in CONFIG.SYS). That makes them
available to utilities for other programs such as Weasel.

:p.These tools are described below.

:dl break=all.

:dt.SelectTNI.cmd
:dd.This decides whether to use an INI or a TNI file, using the same
rules that Major Major uses.

:dt.INIget.cmd
:dd.This fetches a value from an INI or TNI file. It also has options
to fetch a list of all applications, or all keys for a given application.

:dt.INIput.cmd
:dd.This stores a value into an INI or TNI file.

:dt.INIdel.cmd
:dd.This deletes an item from an INI or TNI file. It also has options
to delete all keys or all applications.

:edl.

:p.:hp2.INIData:ehp2.

:p.INIData was alreay mentioned in the :link reftype=hd refid=prerequisites.prerequisites:elink.
section of this manual. Presumably you already have it, or Major Major
would not be working for you. It is mentioned here only for the sake of
completeness.

:p.The Dynamic Link Library (DLL) called INIDATA.DLL is a set of functions
that let the programmer add, modify, or delete entries in an INI or TNI
file. The advantage to the programmer is that the same function calls
work equally well for the INI or TNI format. These functions work out which is
which by looking at the file name: whether it ends in ".INI" or ".TNI".

:p.The advantage to the end user is that, if you have several programs that
all need the same DLL, only one copy of the DLL is loaded into memory.
This might seem unimportant now that large memories are so common, but in
practice we find that a lot of software uses more memory as the memory
sizes grow.

:p.INIDATA.DLL itself requires the presence of another DLL called XDS230M.DLL.
This is part of the runtime support for the compiler used to implement
Major Major, and it is redistributable under mild conditions. That second
DLL is included in the INIData package.

:p.These two DLLs should be placed in a directory that is on your
LIBPATH.  On a system using RPM\YUM, for example ArcaOS, the appropriate directory is
\usr\local\lib. See the README file that comes with the INIData package
for the details. Otherwise, check the LIBPATH definition in your
CONFIG.SYS, and choose one of the directories mentioned there.


:h1.Who was Major Major?

:hp2.Who was Major Major?:ehp2.

:p.Major Major's full name was Major Major Major. He was given
that name by his father. Here is a quote from the novel
:hp1.Catch-22:ehp1., by Joseph Heller.

:lm margin=6.
:rm margin=10.
:font facename='Helv' size=14x8.

:p.A lesser man might have wavered that day in the hospital
corridor, a weaker man might have compromised on such excellent
substitutes as Drum Major, Minor Major, Sergeant Major, or
C. Sharp Major, but Major Major's father had waited fourteen
years for just such an opportunity, and he was not a person
to waste it.
:rm margin=1.
:lm margin=1.
:font facename=default.

:p.When Major Major grew up, he joined the Army. Four days
later, he was promoted to the rank of Major by a computer error.
As a result, his full name became Major Major Major Major.

:p.:hp1.Catch-22:ehp1.  is one of the great books of 20th
century English literature. If you haven't yet read it,
try to find a copy.

:euserdoc.

