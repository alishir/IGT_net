#ifndef FRAMEGRABBER_H
#define FRAMEGRABBER_H

#include <QThread>
#include <QtGui>  // to use qDebug() ... so funny!
#include <QMutex>

#include "cv.h"
#include "highgui.h"

class FrameGrabber : public QThread
{
    Q_OBJECT
public:
    FrameGrabber(QMutex *frameMutex, bool *dataReady, QObject *parent);
    ~FrameGrabber();
protected:
    void run();

protected slots:
    void grab();

signals:
   void frameReady(IplImage *frame);

private:
    QTimer *timer;
    CvCapture *capture;
    IplImage *frame;
    QMutex *frameMutex;
    bool *dataReady;

};

#endif // FRAMEGRABBER_H
