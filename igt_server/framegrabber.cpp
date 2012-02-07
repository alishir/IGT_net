

#include "framegrabber.h"

FrameGrabber::FrameGrabber(QMutex *frameMutex, bool *dataReady, QObject *parent)
    :QThread(parent)
    ,frameMutex(frameMutex)
    ,timer(0)
    ,dataReady(dataReady)
{
//    cvNamedWindow("mywindow", CV_WINDOW_AUTOSIZE);
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
    emit this->frameReady(this->frame);
}
