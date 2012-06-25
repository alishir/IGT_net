/********************************************************************************
** Form generated from reading UI file 'igtscreen.ui'
**
** Created: Mon Jun 25 09:10:19 2012
**      by: Qt User Interface Compiler version 4.6.2
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_IGTSCREEN_H
#define UI_IGTSCREEN_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QMainWindow>
#include <QtGui/QSpacerItem>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_IGTScreen
{
public:
    QWidget *centralwidget;
    QGridLayout *gridLayout;
    QSpacerItem *horizontalSpacer;
    QLabel *label;
    QSpacerItem *horizontalSpacer_4;
    QSpacerItem *horizontalSpacer_2;
    QSpacerItem *verticalSpacer;
    QSpacerItem *horizontalSpacer_3;
    QLabel *error;
    QSpacerItem *verticalSpacer_2;

    void setupUi(QMainWindow *IGTScreen)
    {
        if (IGTScreen->objectName().isEmpty())
            IGTScreen->setObjectName(QString::fromUtf8("IGTScreen"));
        IGTScreen->setWindowModality(Qt::ApplicationModal);
        IGTScreen->resize(855, 715);
        QSizePolicy sizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(IGTScreen->sizePolicy().hasHeightForWidth());
        IGTScreen->setSizePolicy(sizePolicy);
        IGTScreen->setMinimumSize(QSize(855, 536));
        IGTScreen->setMaximumSize(QSize(16777215, 16777215));
        centralwidget = new QWidget(IGTScreen);
        centralwidget->setObjectName(QString::fromUtf8("centralwidget"));
        centralwidget->setMinimumSize(QSize(559, 0));
        centralwidget->setMaximumSize(QSize(16777215, 160000));
        gridLayout = new QGridLayout(centralwidget);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        horizontalSpacer = new QSpacerItem(97, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(horizontalSpacer, 0, 0, 1, 1);

        label = new QLabel(centralwidget);
        label->setObjectName(QString::fromUtf8("label"));
        label->setEnabled(true);
        label->setMinimumSize(QSize(640, 480));

        gridLayout->addWidget(label, 0, 1, 2, 3);

        horizontalSpacer_4 = new QSpacerItem(96, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(horizontalSpacer_4, 0, 4, 1, 1);

        horizontalSpacer_2 = new QSpacerItem(302, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(horizontalSpacer_2, 2, 1, 1, 1);

        verticalSpacer = new QSpacerItem(20, 262, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout->addItem(verticalSpacer, 1, 2, 2, 1);

        horizontalSpacer_3 = new QSpacerItem(301, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(horizontalSpacer_3, 2, 3, 1, 1);

        error = new QLabel(centralwidget);
        error->setObjectName(QString::fromUtf8("error"));
        QFont font;
        font.setPointSize(14);
        font.setBold(true);
        font.setWeight(75);
        error->setFont(font);
        error->setAlignment(Qt::AlignCenter);

        gridLayout->addWidget(error, 3, 1, 1, 3);

        verticalSpacer_2 = new QSpacerItem(20, 166, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout->addItem(verticalSpacer_2, 4, 2, 1, 1);

        IGTScreen->setCentralWidget(centralwidget);

        retranslateUi(IGTScreen);

        QMetaObject::connectSlotsByName(IGTScreen);
    } // setupUi

    void retranslateUi(QMainWindow *IGTScreen)
    {
        IGTScreen->setWindowTitle(QApplication::translate("IGTScreen", "GT Game", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("IGTScreen", "IGT Screen", 0, QApplication::UnicodeUTF8));
        error->setText(QApplication::translate("IGTScreen", "Valid keys are: A, B, C, D", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class IGTScreen: public Ui_IGTScreen {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_IGTSCREEN_H
