#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QMutex>
#include "clientthread.h"
#include "igtscreen.h"

namespace Ui {
    class MainWindow;
}

class MainWindow : public QMainWindow {
    Q_OBJECT
public:
    MainWindow(QWidget *parent = 0);
    ~MainWindow();

protected:
    void changeEvent(QEvent *e);


private:
    Ui::MainWindow *ui;
    IGTScreen *igt;
    ClientThread *imgThread;

private slots:
    void on_show_clicked();
    void on_igt_closed();

};

#endif // MAINWINDOW_H
