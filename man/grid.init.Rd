\name{grid.init}
\alias{grid.init}
\title{ grid.init }
\description{
 grid.init initializes the GridR Package.
If you use a config file, it is not necessary to add one of the parameters explained below.
}
\usage{
grid.init(confFile=NULL, localTmpDir=NULL, verbose=TRUE, sshRemoteIp=NULL, sshUsername=NULL, sshRemoteDir=NULL, myProxyHost=NULL, myProxyUsername=NULL, credentialName=NULL, myProxyPwd=NULL, myProxyPort=NULL, service=NULL, sshKey=NULL, debug=FALSE, sharedDir=NULL, remoteRPath=NULL, schedulerIp=NULL, schedulerPort=NULL)
}
\arguments{
\item{confFile}{Path to the config file}
  \item{localTmpDir}{ Path to a directory where to store temporal data }
  \item{verbose}{if verbose=TRUE a message is printed if a result is availible}
  \item{sshRemoteIp}{If ssh mode is used, the IP of the remote host is specified here}
  \item{sshUsername}{ If an ssh mode is used, the remote username is specified here }
  \item{sshRemoteDir}{ If an ssh mode is used, the remote temp dir is specified here }
  \item{sshKey}{If an ssh mode with windows is used, the path to the public RSA key is specified here} 
  \item{myProxyHost}{the IP of the Host where a myproxy server is running is specified here }
  \item{myProxyUsername}{the username of the myproxy certificate is specified here}
    \item{credentialName}{the credentialname of the myproxy certificate is specified here, if needed}
  \item{myProxyPwd}{the password of the myproxy certificate is specified here}
  \item{myProxyPort}{the port of the myproxy certificate is specified here, if not the default port is used}
  \item{service}{ Here you can add the default service mode, see ?grid.apply }
  \item{debug}{If TRUE, all files will not be deleted locally and on serverside}
  \item{sharedDir}{Path of the directory where shared vaiables will be loaded}
  \item{remoteRPath}{Path to R, if needed}
  \item{schedulerIp}{If a scheduler should be used, add his IP here}
  \item{schedulerPort}{Port of the scheduler, if used}
}
\details{The easiest way to use GridR is to use a config file.\cr
The name of the config file is "gridr.conf" and it should be placed inside the user home directory or the directory from where R is executed. if another path is used, please specify it with the value \"configFile\"\cr
The content depends on which modes you want to use:\cr

There are different ways to submit a function. The services are only available for Linux servers and it is necessary to setup the system in a way that the user is able to login via ssh on the remote computer without entering a password. Please generate RSA Keys. (see ie. \url{http://www.csua.berkeley.edu/~ranga/notes/ssh_nopass.html} for Linux)\cr
To use the services with Windows, a version which uses Trilead Java SSH is implemented. Please download trilead-ssh2-build212.jar or similar at \url{http://www.trilead.com/Download/Trilead_SSH_for_Java/} and place it to <GridRInstallationDir>/GridR/GridR/
Please generate RSA Keys with Puttygen(ie. \url{http://the.earth.li/~sgtatham/putty/latest/x86/puttygen.exe})
Klick on Generate. This will generate a private/public RSA keypair.
Change the Comment to your local username \@ yourLocalComputer
Click on Conversions/Export OpenSSH Key and save it to the path added to your config file.
Now add the Text in the "Public Key for pasting into OpenSSH authorized\_keys file" to a new line to \~/.ssh/authorized\_keys on the server where to execute GridR

On each Server, R must be added to the PATH environment variable, R\_HOME must be set or remoteRPath must be declared.
If condor modes are used, a link to R must be added to /usr/bin/ or add a line to the config file:
<REMOTERPATH>/path/to/R</REMOTERPATH>\cr
If the condor batch modes are used, the package GridR must be installed on each Cluster-Host.

Availible Modes:

\code{variableSharing}

only variable and function sharing is initialized.\cr
You must enter at least the following lines to the config file or command line:\cr

<GRIDR>												\#start tag, necessary\cr
<SHAREDDIR>/home/user/mlohmeyer/share</SHAREDDIR> 	\#local dir where to put tmp files\cr
<SERVICE>variableSharing</SERVICE>					\#which default service to use by default	\cr
</GRIDR> 											\# end tag\cr

\code{local}

the function is executed locally.\cr
You must enter at least the following lines to the config file or command line:\cr

<GRIDR>									\#start tag, necessary\cr
<LOCALTMPDIR>/home/user</LOCALTMPDIR>   \#local dir where to put tmp files\cr
<SERVICE>local</SERVICE>				\#which default service to use by default	\cr
</GRIDR> 								\# end tag\cr

\code{remote.ssh}

the function is copied with ssh to a single computer and is executed there directly in R.\cr
You must enter at least the following lines to the config file or command line:\cr

<GRIDR>									\#start tag, necessary\cr
<LOCALTMPDIR>/home/user</LOCALTMPDIR>  	\#local dir where to put tmp files\cr
<SSHREMOTEDIR>grid/</SSHREMOTEDIR>		\# remote dir where to put tmp files\cr
<SSHREMOTEIP>ip</SSHREMOTEIP>  			\# ip of the remote host\cr
<SSHUSERNAME>user</SSHUSERNAME>			\#ssh username to login on remote host\cr
<SERVICE>remote.ssh</SERVICE>			\#which default service to use by default	\cr
<SSHKEY>/home/user/.ssh/id\_rsa</SSHKEY>	\#on windows systems or with javaSsh=TRUE you have to specify the path to your public RSA key\cr
</GRIDR> 								\# end tag\cr

\code{condor.ssh}

the function is copied with ssh to a computer which is connected to a condor pool and where submission of jobs is possible.\cr
You must enter at least the following lines to the config file or command line:\cr
<GRIDR>									\#start tag, necessary\cr
<LOCALTMPDIR>/home/user</LOCALTMPDIR>  	\#local dir where to put tmp files\cr
<SSHREMOTEDIR>grid/</SSHREMOTEDIR>		\# remote dir where to put tmp files\cr
<SSHREMOTEIP>ip</SSHREMOTEIP>  			\# ip of the remote host\cr
<SSHUSERNAME>username</SSHUSERNAME>		\#ssh username to login on remote host\cr
<SERVICE>condor.ssh</SERVICE>			\#which default service to use by default	\cr
<SSHKEY>/home/user/.ssh/id\_rsa</SSHKEY>	\#on windows systems or with javaSsh=TRUE you have to specify the path to your public RSA key \cr
<REMOTERPATH>pathToRemoteR</REMOTERPATH>\# if R is not linked to /usr/bin/R on serverside, please add this Tag here
</GRIDR>								\# end tag\cr

\code{scheduler}
if the variable schedulerIp is set, all jobs are started with the use of a scheduler. 
At grid.init() it is checked if there are jobs which are started earlier from another R Session. 
If so they are imported to your active R Session.
You can start the scheduler by copying all files from R.home("library/GridR/GridR/scheduler") 
to the server where the scheduler should run, set up a passwordless ssh-logon from your client, 
to this scheduler-server and from the scheduler-server to all execution machines. 
Download trilead-ssh2-build212.jar or similar at \url{http://www.trilead.com/Download/Trilead_SSH_for_Java/} 
and save it to the lib Directory.
Now you can start the scheduler with ./start.sh <port>.
The scheduler can be restarted with \code{\link{grid.restartScheduler}} and stopped with \code{\link{grid.stopScheduler}}.
In scheduler Mode, all submitted jobs can be stopped by \code{\link{grid.stopJob}} or restarted by \code{\link{grid.restartJob}}   
}
\author{ Malte Lohmeyer }

\seealso{ 
\code{\link{grid.apply}} 
\code{\link{GridR}}
\code{\link{grid.share}}

}
\examples{
a<-function(s){return(2*s)}
#define a function that will be executed remotely
library("GridR")
#load the gridR-Code
grid.init(service="local", localTmpDir="GridRTmp/")
#initializes gridR with the parameters entered in the config file
grid.apply("x",a, 3, wait=TRUE)
#applies function `a` with parameter 3 and writes the result to variable x. until the function is executed, x has a lock.
x
grid.apply("y", sum,1:5, wait=TRUE, check=FALSE) # if internal functions are used, its important to set check=FALSE, otherwise the codetools package returns an error
y
}
\keyword{methods }
