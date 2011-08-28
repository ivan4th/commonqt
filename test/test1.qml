import QtQuick 1.0

Rectangle {
    id: main
    width: 100
    height: 100

    FooBar {
        id: fb
        value: {
            var qqq = testobj.someprop * 100;
            console.log("value becomes " + qqq);
            testobj.noteChange(qqq);
            return qqq;
        }
    }

    Component.onCompleted: {
      console.log("testobj: " + testobj);
      testobj.update(123);
      var v1 = testobj.mul10(123);
      var v2 = testobj.concat("abc", "def");
      testobj.done(v1, v2, fb.value);
    }
}
