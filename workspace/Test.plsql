PROCEDURE Fetch_External_Tax (
   order_no_          IN VARCHAR2,
   address_changed_   IN VARCHAR2 DEFAULT 'FALSE',
   include_charges_   IN VARCHAR2 DEFAULT 'TRUE' )
IS
   i_                      NUMBER := 1;
   company_                VARCHAR2(20);
   line_source_key_arr_    Tax_Handling_Util_API.source_key_arr;
   
   CURSOR get_Test IS
   SELECT * FROM Student INNER JOIN Course ON Student.CourseId = Course.CourseId;
   
   CURSOR get_test1 IS
   SELECT * FROM Student S, Course C WHERE S.CourseId(+) = C.CourseId;
BEGIN
   company_                  := Site_API.Get_Company(Get_Contract(order_no_)); 
   line_source_key_arr_.DELETE;
  
   FOR rec_ IN get_order_lines LOOP
      line_source_key_arr_(i_) := Tax_Handling_Util_API.Create_Source_Key_Rec(Tax_Source_API.DB_CUSTOMER_ORDER_LINE,
                                                                              order_no_, 
                                                                              rec_.line_no, 
                                                                              rec_.rel_no, 
                                                                              rec_.line_item_no, 
                                                                              '*',                                                                  
                                                                              attr_ => NULL);

     i_ := i_ + 1;
   END LOOP;

   IF include_charges_ = 'TRUE' THEN 
      FOR rec_ IN get_charge_lines LOOP
         line_source_key_arr_(i_) := Tax_Handling_Util_API.Create_Source_Key_Rec(Tax_Source_API.DB_CUSTOMER_ORDER_CHARGE,
                                                                                 order_no_, 
                                                                                 rec_.sequence_no, 
                                                                                 '*', 
                                                                                 '*', 
                                                                                 '*',                                                                  
                                                                                 attr_ => NULL);

        i_ := i_ + 1;
      END LOOP;
   END IF;

   IF line_source_key_arr_.COUNT >= 1 THEN 
      Tax_Handling_Order_Util_API.Fetch_External_Tax_Info(line_source_key_arr_,
                                                          company_);
   END IF; 

   Customer_Order_History_Api.New(order_no_, Language_Sys.Translate_Constant(lu_name_,'EXTAXBUNDLECALL: External Taxes Updated'));
   
END Fetch_External_Tax;
