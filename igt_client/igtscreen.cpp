#include "igtscreen.h"
#include "ui_igtscreen.h"



IGTScreen::IGTScreen(IGTSubject *sub, QString srvIP, int port, QString savePath, QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::IGTScreen),
    serverIP(srvIP),
    port(port),
    subject(sub),
    savePath(savePath)
{
//    eyeTrackerArgs << "-f";
    this->eyeTracker = new QProcess(this);
    eyeTrackerArgs << "-f";
    eyeTrackerArgs << "video4linux2";
    eyeTrackerArgs << "-s";
    eyeTrackerArgs << "320x240";
    eyeTrackerArgs << "-r";
    eyeTrackerArgs << "30";
    eyeTrackerArgs << "-i";
    eyeTrackerArgs << "/dev/video";
    eyeTrackerArgs << "-f";
    eyeTrackerArgs << "oss";
    eyeTrackerArgs << "-i";
    eyeTrackerArgs << "/dev/dsp";
    eyeTrackerArgs << "-f";
    eyeTrackerArgs << "avi";
    if (savePath.at(savePath.size() - 1) != '/')
    {
        savePath.append("/");
    }
    eyeTrackerArgs << (savePath + QString::number(sub->getId()) + QString(".avi"));

    qDebug() << eyeTrackerArgs;
    this->eyeTracker->start("/usr/bin/ffmpeg", eyeTrackerArgs);
    if (!this->eyeTracker->waitForStarted(500))
    {
        qDebug() << "eye tracker problem ...";
    }

    ui->setupUi(this);
    this->dataSocket = new QTcpSocket(this);
    connect(this->dataSocket, SIGNAL(connected()), this, SLOT(handleConnection()));
    this->dataSocket->connectToHost(this->serverIP, this->port);
    this->time = new QTime();
    this->time->start();
}

IGTScreen::~IGTScreen()
{
    qDebug() << "UI Dead!";
    delete ui;
}

void IGTScreen::changeEvent(QEvent *e)
{
    QMainWindow::changeEvent(e);
    switch (e->type()) {
    case QEvent::LanguageChange:
        ui->retranslateUi(this);
        break;
    default:
        break;
    }
}

void IGTScreen::keyPressEvent(QKeyEvent *event)
{
    char selectedDeck = 'x';
    if (event->text().size())
    {
        selectedDeck = event->text().at(0).toLatin1();
    }
    else
    {
        return;
    }

    qDebug() << "selected Deck: " << selectedDeck;

    switch(selectedDeck)
    {
    case 'a':
    case 'A':
    case 'b':
    case 'B':
    case 'c':
    case 'C':
    case 'd':
    case 'D':
        this->subject->addAction(selectedDeck, this->time->elapsed());
        this->time->restart();
        this->dataSocket->write(&selectedDeck, 1);
        this->dataSocket->flush();
        this->ui->error->setVisible(false);
        break;
    default:
        this->ui->error->setVisible(true);
        break;
    }
}

void IGTScreen::closeEvent(QCloseEvent *e)
{
    e->accept();
    this->subject->save(savePath);
    this->dataSocket->disconnectFromHost();
    delete this->subject;
    this->eyeTracker->close();
    if (!this->eyeTracker->waitForFinished(3000))
    {
        qDebug() << "eye tracker still running ...";
    }
    emit this->destroyed();
}

void IGTScreen::handleConnection()
{
    qDebug() << "Connected to Data Server";
}

void IGTScreen::showWebCam(QPixmap image)
{
    this->ui->label->setPixmap(image);
}

void IGTScreen::setSubject(IGTSubject *sub)
{
    this->subject = sub;
}
