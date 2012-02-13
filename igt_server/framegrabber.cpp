

#include "framegrabber.h"

FrameGrabber::FrameGrabber(QMutex *frameMutex, bool *dataReady, QObject *parent)
    :QThread(parent)
    ,frameMutex(frameMutex)
    ,timer(0)
    ,dataReady(dataReady)
    ,play(false)
	,offFrame(0)
{
    this->cin = new QTextStream(stdin, QIODevice::ReadOnly);
    this->notifier = new QSocketNotifier(STDIN_FILENO, QSocketNotifier::Read, this);
    connect(this->notifier, SIGNAL(activated(int)), this, SLOT(readStdin(int)));
}

void FrameGrabber::readStdin(int)
{
    QString cmd = cin->readLine();
    if (cmd.at(0) == 'P' || cmd.at(0) == 'p')       // start playing the game
    {
        qDebug() << "Playing the Game ...";
        this->play = true;
    }
    if (cmd.at(0) == 'S' || cmd.at(0) == 's')
    {
        qDebug() << "Stoped ...";
        this->play = false;
    }
}

FrameGrabber::~FrameGrabber()
{
    cvReleaseCapture(&(this->capture));
//    cvDestroyWindow("mywindow");
    delete this->timer;
}

void FrameGrabber::run()
{
    this->capture = cvCaptureFromCAM(CV_CAP_ANY);
    if (timer == 0)
    {
        qDebug() << "Create Timer!";
        timer = new QTimer(this);
        connect(this->timer, SIGNAL(timeout()), this, SLOT(grab()));
    }
    timer->start(110);
    exec();
}

void FrameGrabber::grab()
{
//    qDebug() << "Start of Grabbing ....";
    if (!(this->capture)) {
        qDebug() << "ERROR: capture is NULL \n";
    }
    // Create a window in which the captured images will be presented
    // Show the image captured from the camera in the window and repeat
    // Get one frame
    this->frameMutex->lock();
    if (!(*(this->dataReady)))
    {
        this->frame = cvQueryFrame(this->capture);
        if (this->frame == 0)
        {
            qDebug() << "some error in grabbing image";
        }
        else
        {
            *(this->dataReady) = true;
        }
    }
    this->frameMutex->unlock();

    if (this->play)
    {
        emit this->frameReady(this->frame);
    }
    else
    {
        cvCircle(this->frame, cvPoint(this->frame->width / 2,this->frame->height / 2), 5, cvScalar(75,75,75), 12);
        emit this->frameReady(this->frame);
    }
}
