import QtQuick 1.0

Rectangle {
    id: main
    width: 100
    height: 100

    Item {
        id: fb
        function v () {
            var qqq = testobj.someprop * 100;
            console.log("value becomes " + qqq);
            testobj.noteChange(qqq);
            return qqq;
        }
        property double value: v()
    }

    Component.onCompleted: {
      console.log("testobj: " + testobj + " -- " + testobj.self());
      testobj.update(123);
      var v1 = testobj.mul10(123);
      var v2 = testobj.concat("abc", "def")
      // there seems to be a problem with comparing QObjects...
      if (testobj.self().concat("abc", "def") != v2)
        v2 = "PTRFAIL";
      testobj.someprop = 4242;
      if (testobj.someprop != 4242)
        v2 = "PROPFAIL";
      testobj.done(v1, v2, fb.value);
    }
}
