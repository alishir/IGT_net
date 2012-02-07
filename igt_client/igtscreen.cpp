#include "igtscreen.h"
#include "ui_igtscreen.h"



IGTScreen::IGTScreen(IGTSubject *sub, QString srvIP, int port, QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::IGTScreen),
    serverIP(srvIP),
    port(port),
    subject(sub)
{
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
    this->subject->save("/tmp/igt/");
    this->dataSocket->disconnectFromHost();
    delete this->subject;
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
