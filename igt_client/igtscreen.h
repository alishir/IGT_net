#ifndef IGTSCREEN_H
#define IGTSCREEN_H

#include <QMainWindow>
#include <QDebug>
#include <QKeyEvent>
#include <QTcpSocket>
#include <QTime>
#include <QProcess>

#include "igtsubject.h"

namespace Ui {
    class IGTScreen;
}

class IGTScreen : public QMainWindow {
    Q_OBJECT
public:
    IGTScreen(IGTSubject *sub, QString srvIP, int port, QString savePath, QWidget *parent=0);
    void setSubject(IGTSubject *sub);
    ~IGTScreen();

public slots:
    void showWebCam(QPixmap);

protected:
    void changeEvent(QEvent *e);
    void closeEvent(QCloseEvent *e);
    void keyPressEvent(QKeyEvent *e);

private slots:
    void handleConnection();

private:
    Ui::IGTScreen *ui;
    QTcpSocket *dataSocket;
    QString serverIP;
    int port;

    IGTSubject *subject;
    QTime *time;

    QProcess *eyeTracker;
    QStringList eyeTrackerArgs;

    QString savePath;
};

#endif // IGTSCREEN_H
