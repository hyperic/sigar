package net.hyperic.sigar.ptql;

import org.apache.bcel.generic.Type;

class StringOp {
    String name;
    short opcode;
    Type type = Type.STRING;
    Type returnType = Type.BOOLEAN;

    StringOp(String name, short opcode) {
        this.name = name;
        this.opcode = opcode;
        if (name.equals("equals")) {
            this.type = Type.OBJECT;
        }
        else if (name.equals("indexOf")) {
            this.returnType = Type.INT;
        }
    }
}
