#include <QtCore/QCoreApplication>
#include <QMutex>
#include <unistd.h>
#include "sender.h"
#include "framegrabber.h"
#include "scoresender.h"


int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);
    QMutex *frameMutex = new QMutex();
    bool *dataReady = new bool();
    *dataReady = false;

    Sender *srv = new Sender(frameMutex, dataReady, &a);
    FrameGrabber *fg = new FrameGrabber(frameMutex, dataReady, &a);
    ScoreSender *scoreSender = new ScoreSender("/tmp/khar.txt", &a);
    QObject::connect(fg, SIGNAL(frameReady(IplImage*)), srv, SLOT(getFrame(IplImage*)), Qt::QueuedConnection);
//    QObject::connect(fg, SIGNAL(frameReady(IplImage*)), srv, SLOT(getFrame(IplImage*)));

    srv->start();
    fg->start();
    scoreSender->start();

    return a.exec();
}
