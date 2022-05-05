var searchIndex = JSON.parse('{\
"basic_rs":{"doc":"A learning project to build a rudimentary BASIC …","t":[4,13,13,3,13,3,11,11,11,11,11,11,12,0,0,11,11,11,11,12,12,11,11,11,0,12,5,5,11,0,11,11,11,11,11,11,11,11,11,11,12,13,13,13,4,13,4,13,4,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,11,11,11,11,11,11,11,11,11,11,5,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,14,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,8,13,6,6,4,16,3,13,3,13,3,13,6,3,13,6,8,12,11,11,11,11,11,11,11,11,11,11,11,5,12,12,12,12,12,12,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,12,12,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,10,12,12,12,12,12,12,13,13,13,13,13,4,13,13,13,13,11,11,5,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,8,17,5,10,5,5,14],"n":["ControlType","End","Increment","InstructionReader","NoIncrement","State","borrow","borrow","borrow","borrow_mut","borrow_mut","borrow_mut","ctx_vars","data","error","from","from","from","get_instruction","instructions","interactive","into","into","into","keyword","line","main","match_next_for","new","parser","read_instruction","try_from","try_from","try_from","try_into","try_into","try_into","type_id","type_id","type_id","vars","Add","Boolean","Boolean","DataType","Divide","EmptyDataType","Equals","Expression","Float","Float","GreaterThan","GreaterThanEquals","Integer","Integer","LessThan","LessThanEquals","Literal","Multiply","NotEquals","Number","Power","String","String","Subtract","Variable","add","borrow","borrow","borrow","borrow_mut","borrow_mut","borrow_mut","check_type","clone","clone_into","combine","div","eq","eval","extract","fmt","fmt","fmt","from","from","from","get_empty","into","into","into","is_numeric","is_type","matching_types","mul","ne","parser","parser","pow","sub","to_owned","to_string","try_from","try_from","try_from","try_into","try_into","try_into","type_error","type_id","type_id","type_id","typed_eq","typed_ge","typed_gt","typed_le","typed_lt","typed_ne","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","BasicError","Bubble","DataResultKind","DataTypeResult","ErrorKind","IntoErr","IoError","IoError","NameError","NameError","ParseError","ParseError","ResultKind","TypeError","TypeError","TypeResult","WrapExtError","actual","borrow","borrow","borrow","borrow","borrow","borrow_mut","borrow_mut","borrow_mut","borrow_mut","borrow_mut","bubble","bubble_result","context","context","context","context","error","expected","fmt","fmt","fmt","fmt","fmt","from","from","from","from","from","from","from","from","from","into","into","into","into","into","message","name","to_string","to_string","to_string","to_string","to_string","try_from","try_from","try_from","try_from","try_from","try_into","try_into","try_into","try_into","try_into","type_id","type_id","type_id","type_id","type_id","wrap","0","0","0","0","contained","context","Dim","End","For","Goto","IfThen","Keyword","Let","Next","Print","Rem","borrow","borrow_mut","bubble_eval","eval","from","from_str","into","parser","try_from","try_into","type_id","0","0","0","0","0","0","0","0","1","1","1","1","1","1","2","3","4","Parseable","VALID_NAME_CHARS","parse_non_compound_expression","parser","punct","trail_ws","trail_ws"],"q":["basic_rs","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","basic_rs::data","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","basic_rs::data::DataType","","","","basic_rs::data::Expression","","","","","","","","","","","","","","","","","","","","","","","","basic_rs::error","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","basic_rs::error::ErrorKind","","","","","","basic_rs::keyword","","","","","","","","","","","","","","","","","","","","","basic_rs::keyword::Keyword","","","","","","","","","","","","","","","","","basic_rs::parser","","","","","",""],"d":["","","","","","","","","","","","","","Holds types and functions for dealing with data operations.","Error handling types and traits.","Returns the argument unchanged.","Returns the argument unchanged.","Returns the argument unchanged.","Get the instruction recorded at the specified line.","","","Calls <code>U::from(self)</code>.","Calls <code>U::from(self)</code>.","Calls <code>U::from(self)</code>.","","","","","","","Reads the next instruction from the prompt. If reading …","","","","","","","","","","","","","","Holds the available data types.","","Used to hold a specifier for a type of data without the …","","","","","","","","","","","","","","A numerical type, currently <code>Integer</code> or <code>Float</code>.","","","","","","","","","","","","","Similar to <code>is_type()</code>, but returns an <code>Option</code> containing <code>self</code>…","","","","","","","Attempts to extract data from a <code>DataType</code>. May not be very …","","","","Returns the argument unchanged.","Returns the argument unchanged.","Returns the argument unchanged.","Return the <code>EmptyDataType</code> branch corresponding to the type …","Calls <code>U::from(self)</code>.","Calls <code>U::from(self)</code>.","Calls <code>U::from(self)</code>.","Test if <code>self</code> is a numeric type.","Test if <code>self</code> matches the type specified by <code>test_type</code>.","","","","","","","","","","","","","","","","Helper function to get a <code>TypeError</code>.","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","Returns the argument unchanged.","Returns the argument unchanged.","Returns the argument unchanged.","Returns the argument unchanged.","Returns the argument unchanged.","Calls <code>U::from(self)</code>.","Calls <code>U::from(self)</code>.","Calls <code>U::from(self)</code>.","Calls <code>U::from(self)</code>.","Calls <code>U::from(self)</code>.","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","Returns the argument unchanged.","","Calls <code>U::from(self)</code>.","","","","","","","","","","","","","","","","","","","","","","","","","","","",""],"i":[0,1,1,0,1,0,2,1,3,2,1,3,2,0,0,2,1,3,3,3,3,2,1,3,0,2,0,0,3,0,3,2,1,3,2,1,3,2,1,3,2,4,5,6,0,4,0,4,0,5,6,4,4,5,6,4,4,4,4,4,5,4,5,6,4,4,6,4,5,6,4,5,6,6,6,6,0,6,6,4,6,5,6,6,4,5,6,6,4,5,6,6,6,0,6,6,4,6,6,6,6,6,4,5,6,4,5,6,6,4,5,6,6,6,6,6,6,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,13,14,15,16,17,18,19,20,21,22,23,0,24,0,0,0,25,0,24,0,24,0,24,0,0,24,0,0,26,24,26,27,28,29,24,26,27,28,29,30,0,26,27,28,29,29,26,24,26,27,28,29,24,24,24,24,24,26,27,28,29,24,26,27,28,29,28,27,24,26,27,28,29,24,26,27,28,29,24,26,27,28,29,24,26,27,28,29,25,31,32,33,34,35,35,36,36,36,36,36,0,36,36,36,36,36,36,0,36,36,36,36,36,36,36,36,37,38,39,40,41,42,43,44,38,39,41,42,43,44,42,42,42,0,0,0,45,0,0,0],"f":[null,null,null,null,null,null,[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],null,null,null,[[]],[[]],[[]],[[["",0],["usize",0]],["option",4,[["keyword",4]]]],null,null,[[]],[[]],[[]],null,null,[[]],[[["usize",0],["hashmap",3]],["result",4,[["string",3]]]],[[["vec",3,[["string",3]]]],["result",4,[["errorkind",4]]]],null,[[["",0],["usize",0]]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[["",0]],["typeid",3]],[[["",0]],["typeid",3]],[[["",0]],["typeid",3]],null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,[[]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["emptydatatype",4]],["option",4]],[[["",0]],["datatype",4]],[[["",0],["",0]]],[[["result",4,[["errorkind",4]]],["result",4,[["errorkind",4]]],["str",0]],["result",4,[["errorkind",4]]]],[[]],[[["",0],["datatype",4]],["bool",0]],[[["",0],["state",3]],["result",4,[["datatype",4],["errorkind",4]]]],[[],["result",4,[["tryfrom",8],["typeerror",3]]]],[[["",0],["formatter",3]],["result",6]],[[["",0],["formatter",3]],["result",6]],[[["",0],["formatter",3]],["result",6]],[[]],[[]],[[]],[[["",0]],["emptydatatype",4]],[[]],[[]],[[]],[[["",0]],["bool",0]],[[["",0],["emptydatatype",4]],["bool",0]],null,[[]],[[["",0],["datatype",4]],["bool",0]],[[["str",0]],["iresult",6,[["str",0]]]],[[["str",0]],["iresult",6,[["str",0]]]],[[],["result",4,[["datatype",4],["typeerror",3]]]],[[]],[[["",0]]],[[["",0]],["string",3]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[["str",0],["emptydatatype",4]],["result",4,[["typeerror",3]]]],[[["",0]],["typeid",3]],[[["",0]],["typeid",3]],[[["",0]],["typeid",3]],[[],["result",4,[["datatype",4],["typeerror",3]]]],[[],["result",4,[["datatype",4],["typeerror",3]]]],[[],["result",4,[["datatype",4],["typeerror",3]]]],[[],["result",4,[["datatype",4],["typeerror",3]]]],[[],["result",4,[["datatype",4],["typeerror",3]]]],[[],["result",4,[["datatype",4],["typeerror",3]]]],null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["",0]],["",0]],[[["str",0]],["errorkind",4]],[[["result",4,[["basicerror",8]]],["str",0]],["result",4,[["errorkind",4]]]],null,null,null,null,null,null,[[["",0],["formatter",3]],["result",6]],[[["",0],["formatter",3]],["result",6]],[[["",0],["formatter",3]],["result",6]],[[["",0],["formatter",3]],["result",6]],[[["",0],["formatter",3]],["result",6]],[[["nameerror",3]]],[[["typeerror",3]]],[[["ioerror",3]]],[[["parseerror",3]]],[[]],[[]],[[]],[[]],[[]],[[]],[[]],[[]],[[]],[[]],null,null,[[["",0]],["string",3]],[[["",0]],["string",3]],[[["",0]],["string",3]],[[["",0]],["string",3]],[[["",0]],["string",3]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[["",0]],["typeid",3]],[[["",0]],["typeid",3]],[[["",0]],["typeid",3]],[[["",0]],["typeid",3]],[[["",0]],["typeid",3]],[[["str",0]]],null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,[[["",0]],["",0]],[[["",0]],["",0]],[[["expression",4],["state",3]],["result",4,[["datatype",4],["errorkind",4]]]],[[["",0],["state",3]],["result",4,[["controltype",4],["errorkind",4]]]],[[]],[[["str",0]],["result",4]],[[]],[[["str",0]],["iresult",6,[["str",0]]]],[[],["result",4]],[[],["result",4]],[[["",0]],["typeid",3]],null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,[[["str",0]],["iresult",6,[["str",0],["expression",4]]]],[[["str",0]],["iresult",6,[["str",0]]]],[[["str",0]],["iresult",6,[["str",0],["char",0]]]],[[]],null],"p":[[4,"ControlType"],[3,"State"],[3,"InstructionReader"],[4,"Expression"],[4,"EmptyDataType"],[4,"DataType"],[13,"String"],[13,"Integer"],[13,"Float"],[13,"Boolean"],[13,"Variable"],[13,"Literal"],[13,"Add"],[13,"Subtract"],[13,"Multiply"],[13,"Divide"],[13,"Power"],[13,"Equals"],[13,"NotEquals"],[13,"LessThan"],[13,"LessThanEquals"],[13,"GreaterThan"],[13,"GreaterThanEquals"],[4,"ErrorKind"],[8,"WrapExtError"],[3,"TypeError"],[3,"NameError"],[3,"ParseError"],[3,"IoError"],[8,"BasicError"],[13,"TypeError"],[13,"NameError"],[13,"ParseError"],[13,"IoError"],[13,"Bubble"],[4,"Keyword"],[13,"Rem"],[13,"Let"],[13,"Print"],[13,"Goto"],[13,"IfThen"],[13,"For"],[13,"Next"],[13,"Dim"],[8,"Parseable"]]}\
}');
if (window.initSearch) {window.initSearch(searchIndex)};