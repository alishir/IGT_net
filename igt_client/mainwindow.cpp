#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "clientthread.h"
#include <QImage>
#include <QKeyEvent>

MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow),
        igt(0),
        imgThread(0)
{
    ui->setupUi(this);
    this->ui->camPort->setText("12345");
    this->ui->dataPort->setText("2345");
    this->ui->srvIP->setText("172.16.146.206");
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::changeEvent(QEvent *e)
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

void MainWindow::on_igt_closed()
{
    this->show();
    this->imgThread->exit();
    while(this->imgThread->isRunning())
    {
        qDebug() << "Thread is still running ... :(";
    }
    if (this->imgThread->isFinished())
    {
        qDebug() << "Thread has been finished ;)";
        delete this->imgThread;
    }
    this->imgThread = 0;
}

void MainWindow::on_show_clicked()
{
    qDebug() << "Can you see me!";
    QString srvIP = this->ui->srvIP->text();
    int camPort = this->ui->camPort->text().toInt();
    int dataPort = this->ui->dataPort->text().toInt();

    IGTSubject *subject = new IGTSubject();
    subject->setID(this->ui->subID->text().toInt());
    subject->setAge(this->ui->subAge->text().toInt());
    subject->setEMail(this->ui->subMail->text());
    subject->setFamily(this->ui->subFamily->text());
    subject->setGender(this->ui->subGen->currentText());
    subject->setMobile(this->ui->subMobile->text());
    subject->setName(this->ui->subName->text());
    subject->setRorL(this->ui->subRorL->currentText());
    subject->setStdID(this->ui->subStdID->text().toInt());

    if (this->igt == 0)
    {       
        this->igt = new IGTScreen(subject, srvIP, dataPort, this);
        connect(this->igt, SIGNAL(destroyed()), this, SLOT(on_igt_closed()));
    }
    if (this->imgThread == 0)
    {
        this->imgThread = new ClientThread(srvIP, camPort, this);
        connect(this->imgThread, SIGNAL(updateImage(QPixmap)), this->igt, SLOT(showWebCam(QPixmap)));
    }
    this->igt->setSubject(subject);
    igt->show();
    this->hide();
    this->imgThread->start();
}
