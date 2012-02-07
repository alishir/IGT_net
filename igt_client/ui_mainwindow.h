/********************************************************************************
** Form generated from reading UI file 'mainwindow.ui'
**
** Created: Sun Feb 5 19:35:48 2012
**      by: Qt User Interface Compiler version 4.6.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MAINWINDOW_H
#define UI_MAINWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QMainWindow>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_MainWindow
{
public:
    QWidget *centralWidget;
    QHBoxLayout *horizontalLayout_6;
    QSpacerItem *horizontalSpacer_14;
    QVBoxLayout *verticalLayout_3;
    QHBoxLayout *horizontalLayout_5;
    QLabel *label_10;
    QSpacerItem *horizontalSpacer_16;
    QLineEdit *subName;
    QSpacerItem *horizontalSpacer_17;
    QLabel *label_11;
    QSpacerItem *horizontalSpacer_18;
    QLineEdit *subFamily;
    QSpacerItem *verticalSpacer_6;
    QHBoxLayout *horizontalLayout_4;
    QLabel *label;
    QSpacerItem *horizontalSpacer_6;
    QLineEdit *subID;
    QSpacerItem *horizontalSpacer_4;
    QLabel *label_9;
    QSpacerItem *horizontalSpacer_5;
    QLineEdit *subStdID;
    QSpacerItem *verticalSpacer_5;
    QHBoxLayout *horizontalLayout_3;
    QLabel *label_5;
    QSpacerItem *horizontalSpacer_7;
    QLineEdit *subMobile;
    QSpacerItem *horizontalSpacer_8;
    QLabel *label_7;
    QSpacerItem *horizontalSpacer_9;
    QLineEdit *subAge;
    QSpacerItem *horizontalSpacer_15;
    QComboBox *subRorL;
    QSpacerItem *verticalSpacer_4;
    QHBoxLayout *horizontalLayout_2;
    QLabel *label_6;
    QSpacerItem *horizontalSpacer_10;
    QLineEdit *subMail;
    QSpacerItem *horizontalSpacer_11;
    QLabel *label_8;
    QSpacerItem *horizontalSpacer_12;
    QComboBox *subGen;
    QSpacerItem *verticalSpacer_3;
    QVBoxLayout *verticalLayout_2;
    QVBoxLayout *verticalLayout;
    QLabel *label_2;
    QLineEdit *srvIP;
    QSpacerItem *verticalSpacer;
    QHBoxLayout *horizontalLayout;
    QLabel *label_3;
    QSpacerItem *horizontalSpacer_3;
    QLineEdit *camPort;
    QSpacerItem *horizontalSpacer_2;
    QLabel *label_4;
    QSpacerItem *horizontalSpacer;
    QLineEdit *dataPort;
    QSpacerItem *verticalSpacer_2;
    QPushButton *show;
    QSpacerItem *horizontalSpacer_13;

    void setupUi(QMainWindow *MainWindow)
    {
        if (MainWindow->objectName().isEmpty())
            MainWindow->setObjectName(QString::fromUtf8("MainWindow"));
        MainWindow->resize(428, 265);
        centralWidget = new QWidget(MainWindow);
        centralWidget->setObjectName(QString::fromUtf8("centralWidget"));
        horizontalLayout_6 = new QHBoxLayout(centralWidget);
        horizontalLayout_6->setSpacing(6);
        horizontalLayout_6->setContentsMargins(11, 11, 11, 11);
        horizontalLayout_6->setObjectName(QString::fromUtf8("horizontalLayout_6"));
        horizontalSpacer_14 = new QSpacerItem(29, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_6->addItem(horizontalSpacer_14);

        verticalLayout_3 = new QVBoxLayout();
        verticalLayout_3->setSpacing(6);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        horizontalLayout_5 = new QHBoxLayout();
        horizontalLayout_5->setSpacing(6);
        horizontalLayout_5->setObjectName(QString::fromUtf8("horizontalLayout_5"));
        label_10 = new QLabel(centralWidget);
        label_10->setObjectName(QString::fromUtf8("label_10"));

        horizontalLayout_5->addWidget(label_10);

        horizontalSpacer_16 = new QSpacerItem(13, 20, QSizePolicy::Minimum, QSizePolicy::Minimum);

        horizontalLayout_5->addItem(horizontalSpacer_16);

        subName = new QLineEdit(centralWidget);
        subName->setObjectName(QString::fromUtf8("subName"));

        horizontalLayout_5->addWidget(subName);

        horizontalSpacer_17 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_5->addItem(horizontalSpacer_17);

        label_11 = new QLabel(centralWidget);
        label_11->setObjectName(QString::fromUtf8("label_11"));

        horizontalLayout_5->addWidget(label_11);

        horizontalSpacer_18 = new QSpacerItem(13, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_5->addItem(horizontalSpacer_18);

        subFamily = new QLineEdit(centralWidget);
        subFamily->setObjectName(QString::fromUtf8("subFamily"));

        horizontalLayout_5->addWidget(subFamily);


        verticalLayout_3->addLayout(horizontalLayout_5);

        verticalSpacer_6 = new QSpacerItem(20, 13, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_3->addItem(verticalSpacer_6);

        horizontalLayout_4 = new QHBoxLayout();
        horizontalLayout_4->setSpacing(6);
        horizontalLayout_4->setObjectName(QString::fromUtf8("horizontalLayout_4"));
        label = new QLabel(centralWidget);
        label->setObjectName(QString::fromUtf8("label"));

        horizontalLayout_4->addWidget(label);

        horizontalSpacer_6 = new QSpacerItem(13, 20, QSizePolicy::Minimum, QSizePolicy::Minimum);

        horizontalLayout_4->addItem(horizontalSpacer_6);

        subID = new QLineEdit(centralWidget);
        subID->setObjectName(QString::fromUtf8("subID"));

        horizontalLayout_4->addWidget(subID);

        horizontalSpacer_4 = new QSpacerItem(13, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_4->addItem(horizontalSpacer_4);

        label_9 = new QLabel(centralWidget);
        label_9->setObjectName(QString::fromUtf8("label_9"));

        horizontalLayout_4->addWidget(label_9);

        horizontalSpacer_5 = new QSpacerItem(13, 20, QSizePolicy::Minimum, QSizePolicy::Minimum);

        horizontalLayout_4->addItem(horizontalSpacer_5);

        subStdID = new QLineEdit(centralWidget);
        subStdID->setObjectName(QString::fromUtf8("subStdID"));

        horizontalLayout_4->addWidget(subStdID);


        verticalLayout_3->addLayout(horizontalLayout_4);

        verticalSpacer_5 = new QSpacerItem(20, 13, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_3->addItem(verticalSpacer_5);

        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setSpacing(6);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        label_5 = new QLabel(centralWidget);
        label_5->setObjectName(QString::fromUtf8("label_5"));

        horizontalLayout_3->addWidget(label_5);

        horizontalSpacer_7 = new QSpacerItem(13, 20, QSizePolicy::Minimum, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_7);

        subMobile = new QLineEdit(centralWidget);
        subMobile->setObjectName(QString::fromUtf8("subMobile"));

        horizontalLayout_3->addWidget(subMobile);

        horizontalSpacer_8 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_8);

        label_7 = new QLabel(centralWidget);
        label_7->setObjectName(QString::fromUtf8("label_7"));

        horizontalLayout_3->addWidget(label_7);

        horizontalSpacer_9 = new QSpacerItem(13, 20, QSizePolicy::Minimum, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_9);

        subAge = new QLineEdit(centralWidget);
        subAge->setObjectName(QString::fromUtf8("subAge"));

        horizontalLayout_3->addWidget(subAge);

        horizontalSpacer_15 = new QSpacerItem(13, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_15);

        subRorL = new QComboBox(centralWidget);
        subRorL->setObjectName(QString::fromUtf8("subRorL"));

        horizontalLayout_3->addWidget(subRorL);


        verticalLayout_3->addLayout(horizontalLayout_3);

        verticalSpacer_4 = new QSpacerItem(20, 13, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_3->addItem(verticalSpacer_4);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(6);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        label_6 = new QLabel(centralWidget);
        label_6->setObjectName(QString::fromUtf8("label_6"));
        QSizePolicy sizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(label_6->sizePolicy().hasHeightForWidth());
        label_6->setSizePolicy(sizePolicy);

        horizontalLayout_2->addWidget(label_6);

        horizontalSpacer_10 = new QSpacerItem(13, 20, QSizePolicy::Minimum, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_10);

        subMail = new QLineEdit(centralWidget);
        subMail->setObjectName(QString::fromUtf8("subMail"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(subMail->sizePolicy().hasHeightForWidth());
        subMail->setSizePolicy(sizePolicy1);
        subMail->setMinimumSize(QSize(200, 0));
        subMail->setBaseSize(QSize(0, 0));

        horizontalLayout_2->addWidget(subMail);

        horizontalSpacer_11 = new QSpacerItem(18, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_11);

        label_8 = new QLabel(centralWidget);
        label_8->setObjectName(QString::fromUtf8("label_8"));

        horizontalLayout_2->addWidget(label_8);

        horizontalSpacer_12 = new QSpacerItem(13, 20, QSizePolicy::Minimum, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_12);

        subGen = new QComboBox(centralWidget);
        subGen->setObjectName(QString::fromUtf8("subGen"));

        horizontalLayout_2->addWidget(subGen);


        verticalLayout_3->addLayout(horizontalLayout_2);

        verticalSpacer_3 = new QSpacerItem(20, 13, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_3->addItem(verticalSpacer_3);

        verticalLayout_2 = new QVBoxLayout();
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        verticalLayout = new QVBoxLayout();
        verticalLayout->setSpacing(6);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        label_2 = new QLabel(centralWidget);
        label_2->setObjectName(QString::fromUtf8("label_2"));

        verticalLayout->addWidget(label_2);

        srvIP = new QLineEdit(centralWidget);
        srvIP->setObjectName(QString::fromUtf8("srvIP"));

        verticalLayout->addWidget(srvIP);

        verticalSpacer = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        label_3 = new QLabel(centralWidget);
        label_3->setObjectName(QString::fromUtf8("label_3"));

        horizontalLayout->addWidget(label_3);

        horizontalSpacer_3 = new QSpacerItem(13, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_3);

        camPort = new QLineEdit(centralWidget);
        camPort->setObjectName(QString::fromUtf8("camPort"));

        horizontalLayout->addWidget(camPort);

        horizontalSpacer_2 = new QSpacerItem(18, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_2);

        label_4 = new QLabel(centralWidget);
        label_4->setObjectName(QString::fromUtf8("label_4"));

        horizontalLayout->addWidget(label_4);

        horizontalSpacer = new QSpacerItem(17, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        dataPort = new QLineEdit(centralWidget);
        dataPort->setObjectName(QString::fromUtf8("dataPort"));

        horizontalLayout->addWidget(dataPort);


        verticalLayout->addLayout(horizontalLayout);


        verticalLayout_2->addLayout(verticalLayout);

        verticalSpacer_2 = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_2->addItem(verticalSpacer_2);

        show = new QPushButton(centralWidget);
        show->setObjectName(QString::fromUtf8("show"));

        verticalLayout_2->addWidget(show);


        verticalLayout_3->addLayout(verticalLayout_2);


        horizontalLayout_6->addLayout(verticalLayout_3);

        horizontalSpacer_13 = new QSpacerItem(29, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_6->addItem(horizontalSpacer_13);

        MainWindow->setCentralWidget(centralWidget);

        retranslateUi(MainWindow);

        QMetaObject::connectSlotsByName(MainWindow);
    } // setupUi

    void retranslateUi(QMainWindow *MainWindow)
    {
        MainWindow->setWindowTitle(QApplication::translate("MainWindow", "MainWindow", 0, QApplication::UnicodeUTF8));
        label_10->setText(QApplication::translate("MainWindow", "First Name:", 0, QApplication::UnicodeUTF8));
        label_11->setText(QApplication::translate("MainWindow", "Last Name:", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("MainWindow", "ID:", 0, QApplication::UnicodeUTF8));
        label_9->setText(QApplication::translate("MainWindow", "Student ID:", 0, QApplication::UnicodeUTF8));
        label_5->setText(QApplication::translate("MainWindow", "Mobile:", 0, QApplication::UnicodeUTF8));
        subMobile->setText(QString());
        label_7->setText(QApplication::translate("MainWindow", "Age:", 0, QApplication::UnicodeUTF8));
        subRorL->clear();
        subRorL->insertItems(0, QStringList()
         << QApplication::translate("MainWindow", "R", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("MainWindow", "L", 0, QApplication::UnicodeUTF8)
        );
        label_6->setText(QApplication::translate("MainWindow", "E-Mail:", 0, QApplication::UnicodeUTF8));
        subMail->setText(QString());
        label_8->setText(QApplication::translate("MainWindow", "Gender:", 0, QApplication::UnicodeUTF8));
        subGen->clear();
        subGen->insertItems(0, QStringList()
         << QApplication::translate("MainWindow", "male", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("MainWindow", "female", 0, QApplication::UnicodeUTF8)
        );
        label_2->setText(QApplication::translate("MainWindow", "Server:", 0, QApplication::UnicodeUTF8));
        label_3->setText(QApplication::translate("MainWindow", "Cam Port:", 0, QApplication::UnicodeUTF8));
        label_4->setText(QApplication::translate("MainWindow", "Data Port:", 0, QApplication::UnicodeUTF8));
        show->setText(QApplication::translate("MainWindow", "show", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class MainWindow: public Ui_MainWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAINWINDOW_H
