"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        if (typeof b !== "function" && b !== null)
            throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
exports.__esModule = true;
exports.SubChild = void 0;
var SuperParent = /** @class */ (function () {
    function SuperParent() {
    }
    return SuperParent;
}());
var SubChild = /** @class */ (function (_super) {
    __extends(SubChild, _super);
    function SubChild(newSubAttribute7, newSubAttribute8) {
        var _this = _super.call(this) || this;
        _this.newSubAttribute7 = newSubAttribute7;
        _this.newSubAttribute8 = newSubAttribute8;
        _this.newSubAttribute9 = 42;
        return _this;
    }
    return SubChild;
}(SuperParent));
exports.SubChild = SubChild;
