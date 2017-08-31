#include <iostream>
#include <string>
#include <list>

using namespace std;

typedef struct bnc_s
{
	unsigned short  iPort;
	unsigned int	iServerNum;
} bnc;

typedef struct bnc_server_s
{
	std::string			sServer;
	unsigned short  iPort;
	unsigned int	iServerNum;
} bnc_server;

typedef struct bnc_user_s
{
	std::string			sUsername;		// Username
	std::string			sPassword;		// Password
	std::string			sHost;			// Host
	std::string			sIdentd;		// Identd
	unsigned int	iServerNum;
} bnc_user;

typedef struct bnc_login_s
{
	std::string			sUsername;		// Username
	std::string			sIRCUsername;	// Username in IRC
	std::string			sHost;			// Host
	std::string			sIdentd;		// Identd
	unsigned int	iServerNum;
} bnc_login;

unsigned long iServerNum = 0;

bool HandleCommand(std::string pMsg, list<bnc_server> *lsStart) {

	if(!pMsg.compare("add"))
	{
		int iPort = 80;
		bnc_server server;
		server.iServerNum = iServerNum++; 
		server.iPort = iPort;
		server.sServer = "dummy-server";
		lsStart->push_back(server);
	}

	return false;
}

int DeleteServerById(unsigned long iServerNum, std::string szServer, list<bnc_server> *lsStart)
{
	list<bnc_server>::iterator i; for(i=lsStart->begin(); i!=lsStart->end(); ++i)
	//{	if((*i)->iServerNum == iServerNum && !(*i)->sServer.Compare(szServer))
	{	if((*i).iServerNum == iServerNum && !(*i).sServer.compare(szServer))
		{	
			std::cout << "Removing: " << (*i).iServerNum << ": " << (*i).sServer << std::endl;
			//lsStart->remove((*i)); 
			return 0; 
		}
	}
	return 0;
}

int main(void) {
	list<bnc_server> *lsStart;
	lsStart = new list<bnc_server>;
	HandleCommand("add", lsStart);
	HandleCommand("add", lsStart);
	HandleCommand("add", lsStart);
	for(int i = 0; i<iServerNum; i++){
		DeleteServerById(i, "dummy-server", lsStart);
	}
	return 0;
}
