-----------------------------------------------------------------------------
--
--  Logical unit: CustomerAuthorization
--  Component:    ORDER
--
--  IFS Developer Studio Template Version 3.0
--
--  Date    Sign    History
--  ------  ------  ---------------------------------------------------------
-----------------------------------------------------------------------------

layer Cust;

-------------------- PUBLIC DECLARATIONS ------------------------------------


-------------------- PRIVATE DECLARATIONS -----------------------------------


-------------------- LU SPECIFIC IMPLEMENTATION METHODS ---------------------

@Override
PROCEDURE Insert___ (
   objid_      OUT    VARCHAR2,
   objversion_ OUT    VARCHAR2,
   newrec_     IN OUT CUSTOMER_AUTHORIZATION_TAB%ROWTYPE,
   attr_       IN OUT VARCHAR2 )
IS
BEGIN
   super(objid_, objversion_, newrec_, attr_);
 Client_SYS.Add_To_Attr('DATE_OF_APPROVAL', newrec_.date_of_approval ,attr_);
EXCEPTION
   WHEN dup_val_on_index THEN
      Error_SYS.Record_Exist(lu_name_);
END Insert___;


@Override
PROCEDURE Check_Insert___ (
   newrec_ IN OUT customer_authorization_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
   ptr_   NUMBER;
   name_  VARCHAR2(30);
   value_ VARCHAR2(4000);
   --Added By Manav
   Temp   VARCHAR2(30);
   Cnt_   NUMBER;
   CURSOR check_approver(user_id_ VARCHAR2,slab_ VARCHAR2) IS
     SELECT COUNT(*)
     FROM   cust_ord_approver 
		WHERE user_id = user_id_
		AND(  slab    = slab_
		    OR   slab = 'Super User');
   --End Manav
BEGIN
   super(newrec_, indrec_, attr_);
   IF newrec_.date_of_approval IS NULL THEN 
      newrec_.date_of_approval := SYSDATE;
   END IF;
   
   Cnt_ := 0;
   Temp := '';
   CUSTOMER_ORDER_FLOW_API.Approve_Price_List(   newrec_.order_no,Temp);
   IF Temp = 'Min'  THEN
      Temp := 'Less Than Min Price';
   END IF;
   
   IF Temp = 'Base'  THEN
      Temp := 'Less Than Base Price';
   END IF; 
   
   OPEN  check_approver(newrec_.user_id,Temp);
   FETCH check_approver INTO Cnt_;
   CLOSE check_approver;
   
   IF NVL(Cnt_,0) = 0 THEN
      Error_SYS.Record_General(lu_name_, 'INVALIDAPPROVER: Not a Valid Approver');
   END IF;
EXCEPTION
   WHEN value_error THEN
      Error_SYS.Item_Format(lu_name_, name_, value_);
END Check_Insert___;


@Override
PROCEDURE Check_Update___ (
   oldrec_ IN     customer_authorization_tab%ROWTYPE,
   newrec_ IN OUT customer_authorization_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
   ptr_   NUMBER;
   name_  VARCHAR2(30);
   value_ VARCHAR2(4000);
   --Added By Manav
   Temp   VARCHAR2(30);
   Cnt_   NUMBER;
   CURSOR check_approver(user_id_ VARCHAR2,slab_ VARCHAR2) IS
     SELECT COUNT(*)
     FROM   cust_ord_approver 
		WHERE user_id = user_id_
		AND (  slab    = slab_
		 OR    slab    = 'Super User');
   --End Manav

BEGIN
   super(oldrec_, newrec_, indrec_, attr_);
   Cnt_ := 0;
   Temp := '';
   CUSTOMER_ORDER_FLOW_API.Approve_Price_List(   newrec_.order_no,Temp);
   IF Temp = 'Min'  THEN
      Temp := 'Less Than Min Price';
   END IF;
   
   IF Temp = 'Base'  THEN
      Temp := 'Less Than Base Price';
   END IF; 
   
   OPEN  check_approver(newrec_.user_id,Temp);
   FETCH check_approver INTO Cnt_;
   CLOSE check_approver;
   
   IF NVL(Cnt_,0) = 0 THEN
      Error_SYS.Record_General(lu_name_, 'Not a Valid Approver');
   END IF;
EXCEPTION
   WHEN value_error THEN
      Error_SYS.Item_Format(lu_name_, name_, value_);
END Check_Update___;


-------------------- LU SPECIFIC PRIVATE METHODS ----------------------------

-------------------- LU SPECIFIC PROTECTED METHODS --------------------------

-------------------- LU SPECIFIC PUBLIC METHODS -----------------------------


