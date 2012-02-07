/****************************************************************************
** Meta object code from reading C++ file 'igtscreen.h'
**
** Created: Sun Feb 5 19:36:02 2012
**      by: The Qt Meta Object Compiler version 62 (Qt 4.6.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "igtscreen.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'igtscreen.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_IGTScreen[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      11,   10,   10,   10, 0x0a,
      31,   10,   10,   10, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_IGTScreen[] = {
    "IGTScreen\0\0showWebCam(QPixmap)\0"
    "handleConnection()\0"
};

const QMetaObject IGTScreen::staticMetaObject = {
    { &QMainWindow::staticMetaObject, qt_meta_stringdata_IGTScreen,
      qt_meta_data_IGTScreen, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &IGTScreen::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *IGTScreen::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *IGTScreen::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_IGTScreen))
        return static_cast<void*>(const_cast< IGTScreen*>(this));
    return QMainWindow::qt_metacast(_clname);
}

int IGTScreen::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QMainWindow::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: showWebCam((*reinterpret_cast< QPixmap(*)>(_a[1]))); break;
        case 1: handleConnection(); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
