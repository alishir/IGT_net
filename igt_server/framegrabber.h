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
    QTextStream *cin;
    QSocketNotifier *notifier;
    void run();

protected slots:
    void grab();
    void readStdin(int);

signals:
   void frameReady(IplImage *frame);

private:
    QTimer *timer;
    CvCapture *capture;
    IplImage *frame;
    IplImage *offFrame;
    QMutex *frameMutex;
    bool *dataReady;
    bool play;

};

#endif // FRAMEGRABBER_H
