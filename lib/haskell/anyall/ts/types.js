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
exports.NotNode = exports.AllNode = exports.AnyNode = exports.PrePostNode = exports.PreNode = exports.LeafNode = void 0;
var LeafNode = /** @class */ (function () {
    function LeafNode() {
    }
    return LeafNode;
}());
exports.LeafNode = LeafNode;
var PreNode = /** @class */ (function () {
    function PreNode() {
    }
    return PreNode;
}());
exports.PreNode = PreNode;
var PrePostNode = /** @class */ (function () {
    function PrePostNode() {
    }
    return PrePostNode;
}());
exports.PrePostNode = PrePostNode;
var SubTree = /** @class */ (function () {
    function SubTree() {
    }
    return SubTree;
}());
var AnyNode = /** @class */ (function (_super) {
    __extends(AnyNode, _super);
    function AnyNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    return AnyNode;
}(SubTree));
exports.AnyNode = AnyNode;
var AllNode = /** @class */ (function (_super) {
    __extends(AllNode, _super);
    function AllNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    return AllNode;
}(SubTree));
exports.AllNode = AllNode;
var NotNode = /** @class */ (function () {
    function NotNode() {
    }
    return NotNode;
}());
exports.NotNode = NotNode;
