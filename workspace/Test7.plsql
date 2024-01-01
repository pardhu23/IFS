-----------------------------------------------------------------------------
--
--  Logical unit: PurchaseOrder
--  Component:    PURCH
--
--  IFS Developer Studio Template Version 3.0
--
--  Date    Sign    History
--  ------  ------  ---------------------------------------------------------
--  231212  TecVaruSa  M-OS-067-M   : Modified PROCEDURE Cancel__ 
--  231212   TecVaruSa BLK-IMM-02-M : Bug Fixing Modifications
--  231114  TecVaruSa  BLK-IMM-02-M Added C_Get_Associated_Buyer, C_Short_Close_Reason_Update and ovriddedn Prepare_Insert___ and Close__
--  231121  TechSurenS MERGE-ENVECON-1: Merged Envecon Changes
--  231210  TecFarahS  BLK-IMM-31-M-HALP-8680  : Overtaken Restrict_Po_Header_Update__ procedure
--  231025  TecAwantW  BLK-IMM-29-M, Moved Check_Auth_Object_Change from Finite_State_Machine___ to Finite_State_Set__.
--  231011  TecRahulM  BLK-IMM-15-Ma : Changes for Supplier Blacklist
--  230825  TecSurenS  M-BP-001  Business Planning
--  230713  TecFarahS  REQ_819-M : Added CAutoClose Flag column for Ownership Transfer
--  230712  TecVaruSa  M-OS-067-M : Overriden Check_Update___
--  230526  TecVaruSa  M-OS-002_M-OS-053-M: Overriden User_Requisition_Line_To_Order & Overtook User_Proposal_To_Order
--  220914  TecPardhY  BLK-IMM-29-M: Overriden Check_Common___ & Release__ to perfom the supplier status check
-----------------------------------------------------------------------------

layer Cust;

-------------------- PUBLIC DECLARATIONS ------------------------------------

--(+) 231121 TechSurenS MERGE-ENV-ECON-1 (START)

@ApproveGlobalVariable(2016/03/15,smtain)
NO_OF_DECIMALS          NUMBER := 12;

@ApproveGlobalVariable(2016/03/15,smtain)
prefix_			         VARCHAR2(20);

@ApproveGlobalVariable(2016/03/15,smtain)
GTender_No_             VARCHAR2(25) := Null;

@ApproveGlobalVariable(2016/03/15,smtain)
GDoc_No_                VARCHAR2(25) := Null;

@ApproveGlobalVariable(2016/03/15,smtain)
GPrefix_                VARCHAR2(50) := Null;

@ApproveGlobalVariable(2016/03/15,smtain)
GPoPrefix_              VARCHAR2(50) := Null;

@ApproveGlobalVariable(2016/03/15,smtain)
GContract_              VARCHAR2(5)  := NULL;

@ApproveGlobalVariable(2016/03/15,smtain)
GStructure_             VARCHAR2(50) := NULL;

@ApproveGlobalVariable(2016/03/15,smtain)
GPoQty_                 NUMBER       := Null;

@ApproveGlobalVariable(2016/03/15,smtain)
GMultipleRow_           VARCHAR2(5)  := 'FALSE';

@ApproveGlobalVariable(2016/03/15,smtain)
GBidSno_                NUMBER       := Null;

@ApproveGlobalVariable(2016/03/15,smtain)
GDelDate_               DATE          := Null;

@ApproveGlobalVariable(2016/03/15,smtain)
GIndianCharges_         VARCHAR2(5) := 'FALSE';  

--(+) 231121 TechSurenS MERGE-ENV-ECON-1 (FINISH)

-------------------- PRIVATE DECLARATIONS -----------------------------------


-------------------- LU SPECIFIC IMPLEMENTATION METHODS ---------------------

--(+) 231121 TechSurenS MERGE-ENV-ECON-1 (START)

@Override
PROCEDURE Prepare_Insert___ (
   attr_ IN OUT VARCHAR2 )
IS
BEGIN
   super(attr_);
   Client_SYS.Add_To_Attr('INTRASTAT_EXEMPT_DB', 'Include', attr_);      -- India Localisation

   --(+)231114 TecVaruSa BLK-IMM-02-M (START)
   Client_SYS.Add_To_Attr('C_SHORT_CLOSED', 'No', attr_);
   --(+)231114 TecVaruSa BLK-IMM-02-M (FINISH)
END Prepare_Insert___;

@Override
PROCEDURE Check_Insert___ (
   newrec_ IN OUT purchase_order_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
BEGIN
 --  super(newrec_, indrec_, attr_);
   IF newrec_.contract IS NOT NULL THEN
      prefix_ := NULL;    -- India localisation
   END IF;
    prefix_ := Client_Sys.Get_Item_Value('PREFIX',attr_);
   IF newrec_.open_po IS NULL THEN
      newrec_.open_po  := NVL(newrec_.open_po,'N');
   END IF;
   IF (newrec_.delivery_terms IS NOT NULL) THEN
      IF (newrec_.freight_flag IS NULL) THEN
         newrec_.freight_flag := Freight_Flag_API.Encode(nvl(Order_Delivery_Term_API.Get_Freight_Flag(
            newrec_.delivery_terms), newrec_.freight_flag));
      END IF;
   END IF;
   IF newrec_.freight_flag IS NULL THEN
      newrec_.freight_flag := 'N';
   END IF;
   super(newrec_, indrec_, attr_);
   Client_SYS.Add_To_Attr('PREFIX',prefix_,attr_);
EXCEPTION
   WHEN OTHERS THEN
      GPoPrefix_	:= Null;
      RAISE;   
END Check_Insert___;

@Override
PROCEDURE Check_Update___ (
   oldrec_ IN     purchase_order_tab%ROWTYPE,
   newrec_ IN OUT purchase_order_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
   --(+)230712 TecVaruSa M-OS-067-M (Start)
   CURSOR get_labor_class_count IS 
      SELECT COUNT(*)
      FROM  purchase_order_line_tab pol
      WHERE pol.order_no = newrec_.order_no
      AND   pol.c_labor_class_no IS NOT NULL;
      
      count_ NUMBER;
   --(+)230712 TecVaruSa M-OS-067-M (Finish)
BEGIN
   --(+)230712 TecVaruSa M-OS-067-M (Start)
   OPEN  get_labor_class_count;
   FETCH get_labor_class_count INTO count_;
   CLOSE get_labor_class_count;
   
   IF newrec_.c_insourcing_contract <> oldrec_.c_insourcing_contract AND count_ > 0 THEN
      Error_SYS.Record_General(lu_name_, 'CINSOURCINGCONTRACTUPD: Insourcing Contract Cannot be changed if Lobor Class is created.');
   END IF;   
   --(+)230712 TecVaruSa M-OS-067-M (Finish)
   --(+) 230704  TecAwantW BLK-IMM-29-M(START)
   $IF Component_Cmod_SYS.INSTALLED $THEN 
      C_Authorization_Header_API.Check_Auth_Object_Change(Purchase_Order_API.lu_name_, 
                                                          newrec_.rowkey);
   $END 
   --(+) 230704  TecAwantW BLK-IMM-29-M(FINISH)
   super(oldrec_, newrec_, indrec_, attr_);
   --(+) 231121 TechSurenS MERGE-ENV-ECON-1 (START)
   IF newrec_.open_po IS NOT NULL  AND nvl(oldrec_.open_po,'$$') != nvl(newrec_.open_po,'$$') THEN
		IF (Purchase_Order_API.Get_Objstate__(newrec_.order_no) NOT IN ('Planned')) THEN
         Error_SYS.Record_General(lu_name_, 'OPENPOCHK: Cannot update OPEN PO once the state is Released');
      END IF;
   END IF;
   -- ADDED BY GIRISH 13-FEB-2003	 BEGIN
   IF NVL(newrec_.open_po,'N') = 'Y' THEN
		Update_PoOrderLines(newrec_.order_no);
   END IF;
   --<start> SHMOIN <C_Pending Issues for Closure -PNC-1>
   IF newrec_.rowstate ='Planned' AND nvl(Site_Ind_Param_API.Get_Pr_Security(newrec_.contract),'FALSE') = 'TRUE' THEN
      purchase_order_api.Check_Dept_Access(newrec_.order_no,newrec_.contract);
   END IF;
   --<end> SHMOIN <C_Pending Issues for Closure -PNC-1>

   IF newrec_.open_po IS NULL THEN
      newrec_.open_po  := NVL(newrec_.open_po,'N');
   END IF;
   --(+) 231121 TechSurenS MERGE-ENV-ECON-1 (FINISH)
END Check_Update___;

@Override
PROCEDURE Update___ (
   objid_      IN     VARCHAR2,
   oldrec_     IN     PURCHASE_ORDER_TAB%ROWTYPE,
   newrec_     IN OUT PURCHASE_ORDER_TAB%ROWTYPE,
   attr_       IN OUT VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   by_keys_    IN     BOOLEAN DEFAULT FALSE )
IS
BEGIN
   super(objid_, oldrec_, newrec_, attr_, objversion_, by_keys_);
   IF Nvl(newrec_.addr_no,'!@#') <> Nvl(oldrec_.addr_no,'!@#') THEN
      PURCH_STRUC_INSERTION_API.MODIFY_TAX_RATE(newrec_.order_no, NULL,'PO');
   END IF;   
END Update___;
--(+) 231121 TechSurenS MERGE-ENV-ECON-1 (FINISH)

--(+)220914 TecPardhY BLK-IMM-29-M (START)
@Override
PROCEDURE Check_Common___ (
   oldrec_ IN     purchase_order_tab%ROWTYPE,
   newrec_ IN OUT purchase_order_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
BEGIN
   --(+)220914 TecPardhY BLK-IMM-29-M (START)
   IF Validate_SYS.Is_Changed(oldrec_.vendor_no, newrec_.vendor_no) AND Supplier_Info_API.Get_C_Status_Db(newrec_.vendor_no) <> 'APPROVED' THEN
      Error_SYS.Record_General(lu_name_, 'CSUPERR: Supplier :P1 is not in approved state', newrec_.vendor_no);
   END IF;
   --(+)220914 TecPardhY BLK-IMM-29-M (FINISH)
   -- (+) 231011  TecRahulM  BLK-IMM-15-Ma (START)  
   $IF Component_Cmod_SYS.INSTALLED $THEN 
   IF Validate_SYS.Is_Changed(oldrec_.vendor_no, newrec_.vendor_no) AND C_Supplier_Debar_API.Check_Supplier_Comp_Debar(newrec_.vendor_no,newrec_.contract) = 1 THEN
      Error_SYS.Record_General(lu_name_, 'CSUPERR: Supplier :P1 is Debarred/Cancel Registered', newrec_.vendor_no);
   END IF;
   $END
   -- (+) 231011  TecRahulM  BLK-IMM-15-Ma (FINISH)   
   super(oldrec_, newrec_, indrec_, attr_);
END Check_Common___;
--(+)220914 TecPardhY BLK-IMM-29-M (FINISH)

@Override
PROCEDURE Finite_State_Machine___ (
   rec_   IN OUT purchase_order_tab%ROWTYPE,
   event_ IN     VARCHAR2,
   attr_  IN OUT VARCHAR2 )
IS
BEGIN
   super(rec_, event_, attr_);
   --(+)230705  TecFarahS  REQ_819-M(START)
   IF (rec_.rowstate = 'Released' AND event_ IS NULL AND rec_.c_auto_close='Yes') THEN
      Temporary_Part_Ownership_API.C_Auto_Close(rec_.order_no);
   END IF;
   --(+)230705  TecFarahS  REQ_819-M(FINISH)
END Finite_State_Machine___;

@Override
PROCEDURE Finite_State_Set___ (
   rec_   IN OUT PURCHASE_ORDER_TAB%ROWTYPE,
   state_ IN     VARCHAR2 )
IS
BEGIN
   --(+) 231020  TecAwantW BLK-IMM-29-M(START)
   $IF Component_Cmod_SYS.INSTALLED $THEN 
      C_Authorization_Header_API.Check_Auth_Object_Change(lu_name_         => Purchase_Order_API.lu_name_, 
                                                          source_objkey_   => rec_.rowkey,
                                                          is_state_change_ => Fnd_Boolean_API.DB_TRUE,
                                                          target_state_    => state_);
   $END 
   --(+) 231020  TecAwantW BLK-IMM-29-M(FINISH)
   super(rec_, state_);
   
END Finite_State_Set___;

-------------------- LU SPECIFIC PRIVATE METHODS ----------------------------
--(+) 231121 TechSurenS MERGE-ENV-ECON-1 (START)
@Overtake 
PROCEDURE Copy__ (
   info_                   IN OUT NOCOPY VARCHAR2,
   destination_order_      IN OUT NOCOPY VARCHAR2,
   new_delivery_date_      IN OUT NOCOPY DATE,
   order_no_               IN     VARCHAR2,
   new_supplier_           IN     VARCHAR2,
   new_blanket_            IN     VARCHAR2,
   include_doc_texts_      IN     VARCHAR2,
   include_internal_notes_ IN     VARCHAR2,
   include_pre_accounting_ IN     VARCHAR2 )
IS
   $SEARCH
   po_contract_  PURCHASE_ORDER_TAB.contract%TYPE;
   $APPEND
    CURSOR Get_Struc (Order_No_   IN VARCHAR2,
   			  Line_No_ 	  IN VARCHAR2,
   			  Release_No_ IN VARCHAR2) IS
   	SELECT Distinct STRCODE
   	From   Purch_line_Struc_Tab
   	Where  Order_No = Order_No_
   	And    Line_no =  Line_No_
   	And    Release_No = Release_No_;

	StrCode_ VARCHAR2(50);

	--- C_G1312391-1  Start
	 CURSOR Get_def_Struc IS
	    SELECT a.strcode from
	       SUPP_DIST_TAX_HEADER  a
	     where a.supplier_id = new_supplier_
	     and a.address_id = purchase_order_api.Get_Addr_No(destination_order_)
	     and a.default_flag = 'TRUE'
            and a.objstate = 'Firm';
   $END
BEGIN
   $SEARCH
         FOR line_rec in orderlines LOOP
   $APPEND
        Purchase_Order_Api.GStructure_  	:= null;
      StrCode_ 		:= Null;
      Trace_Sys.Field('Old Vendor : ',Purchase_Order_Api.Get_Vendor_No (destination_order_));
      Trace_Sys.Field('New Vendor : ',Purchase_Order_Api.Get_Vendor_No (order_no_));
       Purchase_Order_Api.GStructure_ := NULL; --- C_G1312391-1
      IF Purchase_Order_API.Get_Vendor_No (destination_order_) = Purchase_Order_API.Get_Vendor_No (order_no_) THEN
         OPEN  Get_Struc (Order_No_,Line_Rec.Line_no,line_rec.release_no);
         FETCH Get_Struc INTO StrCode_;
         CLOSE Get_Struc;
         Trace_SYS.Field('<<<< Structure >>>> ',StrCode_);
         IF StrCode_ IS NOT NULL THEN
            Purchase_Order_API.GStructure_ := StrCode_;
         END IF;
         --- C_G1312391-1 Start
      ELSE
         OPEN  Get_def_Struc ;
         FETCH Get_def_Struc INTO StrCode_;
         CLOSE Get_def_Struc;
         Trace_SYS.Field('<<<< Structure >>>> ',StrCode_);
         IF StrCode_ IS NOT NULL THEN
            Purchase_Order_API.GStructure_ := StrCode_;
         END IF;
         --- C_G1312391-1 End
      END IF;
   $END
END Copy__;


@Overtake Core
PROCEDURE Copy_Order_Insert___ (
   objid_      OUT    VARCHAR2,
   objversion_ OUT    VARCHAR2,
   newrec_     IN OUT NOCOPY PURCHASE_ORDER_TAB%ROWTYPE,
   attr_       IN OUT NOCOPY VARCHAR2 )
IS
     prefix_no_ NUMBER;
BEGIN
        newrec_.rowversion := sysdate;
      objversion_ := to_char(newrec_.rowversion,'YYYYMMDDHH24MISS');
   
      newrec_.order_no := Get_New_Order_No___(newrec_.order_no, newrec_.authorize_code, NULL);
      Error_SYS.Check_Not_Null(lu_name_, 'ORDER_NO', newrec_.order_no);
   
      newrec_.note_id := Document_Text_API.Get_Next_Note_Id;
      Client_SYS.Add_To_Attr('NOTE_ID', newrec_.note_id, attr_);
    
      IF newrec_.project_address_flag IS NULL THEN
         newrec_.project_address_flag := 'N';
      END IF;
   
      Error_SYS.Check_Not_Null(lu_name_, 'NOTE_ID', newrec_.note_id);
      
      Client_SYS.Add_To_Attr('PURCHASE_CODE', newrec_.purchase_code, attr_);
      -- Bug 128341, start
      Client_SYS.Add_To_Attr('COPY_ORDER', 'TRUE', attr_);
      -- Bug 128341, end
   prefix_ := Client_Sys.Get_Item_Value('PREFIX',attr_);
   IF Company_Tax_Info_API.get_purch_prefix_flag(Site_API.get_company(newrec_.contract)) = 'TRUE' THEN
      IF nvl(prefix_,'x') = 'x' THEN
         prefix_ := Site_Prefix_API.get_default_prefix(Site_Site_Group_API.get_site_group(newrec_.contract,'PO'),'PO');
      END IF; 
      Site_Prefix_API.valid_prefix_contract(newrec_.contract,prefix_,'PO');
      Site_Prefix_API.Exist(prefix_);
      Site_Prefix_API.Valid_Prefix(prefix_, newrec_.order_date, 'PO');
      Site_Prefix_API.Get_Next_No(prefix_,prefix_no_);

      newrec_.order_no := prefix_||prefix_no_;

      Prefix_Transaction_API.ins_prefix_txn(newrec_.order_no,
                                            'PO',
                                            prefix_,
                                            prefix_||prefix_no_);
   END IF;
   Client_SYS.Add_To_Attr('PREFIX', prefix_, attr_);   -- India Localisatio      
      Insert___(objid_, objversion_, newrec_, attr_);
     
      Client_SYS.Add_To_Attr('ORDER_NO', newrec_.order_no, attr_);
      Client_SYS.Add_To_Attr('SHIP_VIA_CODE', newrec_.ship_via_code, attr_);
      Client_SYS.Add_To_Attr('ADDR_NO', newrec_.addr_no, attr_);
      Client_SYS.Add_To_Attr('DOC_ADDR_NO', newrec_.doc_addr_no, attr_);
      Client_SYS.Add_To_Attr('DELIVERY_TERMS', newrec_.delivery_terms, attr_);
      Client_SYS.Add_To_Attr('DOCUMENT_ADDRESS_ID', newrec_.document_address_id, attr_);
      Client_SYS.Add_To_Attr('INTRASTAT_EXEMPT_DB', newrec_.intrastat_exempt, attr_);
   EXCEPTION
      WHEN dup_val_on_index THEN
         Raise_Record_Exist___(newrec_);
END Copy_Order_Insert___;




@Override
PROCEDURE Cancel_Lines___ (
   rec_  IN OUT PURCHASE_ORDER_TAB%ROWTYPE,
   attr_ IN OUT VARCHAR2 )
IS
BEGIN
   super(rec_, attr_);
	IF purchase_order_approval_api.any_lines_approved(rec_.order_no,'*')='TRUE' THEN
		Error_SYS.Record_General('lu_name_','PAYEXISTERR: Cancellation Not Allowed. The Purchase Order Is Authorized.');
	END IF;
   Pr_Budget_Details_API.Refresh_PO_Amount(rec_.order_no, NULL, NULL);
END Cancel_Lines___;

@Overtake Core
PROCEDURE Update_Order_Date___ (
   rec_  IN OUT PURCHASE_ORDER_TAB%ROWTYPE,
   attr_ IN OUT VARCHAR2 )
IS
BEGIN
   $SEARCH
      Client_SYS.Clear_Attr(lattr_);
   $REPLACE
      Client_SYS.Clear_Attr(lattr_);
      Client_SYS.Add_To_Attr('REVISION_DATE', Site_API.Get_Site_Date(rec_.contract), lattr_); 
   $END
END Update_Order_Date___;
--(+) 231121 TechSurenS MERGE-ENV-ECON-1 (FINISH)

--(+)220914 TecPardhY BLK-IMM-29-M (START)
@Override
PROCEDURE Release__(info_       OUT    VARCHAR2,
					objid_      IN     VARCHAR2,
					objversion_ IN OUT VARCHAR2,
					attr_       IN OUT VARCHAR2,
					action_     IN     VARCHAR2 )
IS
   --(+)230817  TecJatinG    M-OS-094-M (START)
   c_qty_               NUMBER;
   c_state_             VARCHAR2(32000);
   lu_rec_ purchase_order_tab%ROWTYPE; 
   --(+)230817  TecJatinG    M-OS-094-M (FINISH)
   --(+) 231121 TechSurenS ME
   -- MERGE-ENV-ECON-1 (START)-- India Localisation <Start>

   cnt_  NUMBER;
   var3_ VARCHAR2(10);

   CURSOR C1(order_no_ IN VARCHAR2) IS
    SELECT Company FROM Purchase_Order WHERE Order_No = order_no_;


   CURSOR C2(order_no_ IN VARCHAR2) IS
    SELECT *
      FROM Purchase_Order_Line_Tab
     WHERE Order_no = order_no_
       AND PART_NO IS NOT NULL
       AND Rowstate != 'Cancelled';

   supl_curr_code_       VARCHAR2(10);
   supl_currency_amount_ NUMBER;
   supl_currency_rate_   NUMBER;
   amt_                  NUMBER;
   lcamt_                NUMBER;
   lcno_                 VARCHAR2(20);
   stat_                 VARCHAR2(10);
   bnk_                  VARCHAR2(100);

   line_cost_         NUMBER := 0;
   this_pr_line_cost_ NUMBER := 0;
   used_amount_       NUMBER := 0;
   budgetamount_      NUMBER := 0;
   left_amount_       NUMBER := 0;

   budget_category_ DEPARTMENT_BUDGET_TAB.Budget_Category%TYPE;
   fin_year_        DEPARTMENT_BUDGET_TAB.fin_year%TYPE;
   cost_center_     DEPARTMENT_BUDGET_TAB.cost_center%TYPE;
   company_         DEPARTMENT_BUDGET_TAB.company%TYPE;
   budget_info_ VARCHAR2(2000); 
   rec_  purchase_order_tab%ROWTYPE;
   
   CURSOR GetReqNo(order_no_ VARCHAR2 ) IS
    SELECT DISTINCT T.REQUISITION_NO REQUISITION_NO
      FROM PURCHASE_ORDER_LINE_TAB t
     WHERE t.order_no = order_no_
       AND Purchase_Order_Api.Get_Objstate__(t.order_no) != 'Cancelled'
       AND Purchase_Requisition_Api.Get_Objstate__(t.requisition_no) !=
           'Cancelled'; 

   CURSOR GetPO(requisition_no_ VARCHAR2) IS
    SELECT *
      FROM Purchase_order_Line_tab t
     WHERE t.order_no = rec_.order_no
       AND t.requisition_no = requisition_no_
       AND t.rowstate != 'Cancelled'; 

   CURSOR exist_budget(requisition_no_ VARCHAR2) IS
    SELECT budget_category, fin_year, cost_center, company
      FROM PR_BUDGET_DETAILS_TAB
     WHERE requisition_no = requisition_no_
       AND Purchase_Requisition_Api.Get_Objstate__(REQUISITION_NO) !=
           'Cancelled'; 

   acquisition_site_    purchase_order_tab.contract%TYPE;     --  170217  ANJRIN   <C_G1768297-1 GST Development> start 
   --(+) 231121 TechSurenS MERGE-ENV-ECON-1 (FINISH)

   --(+)220914 TecPardhY BLK-IMM-29-M (START)
   c_supplier_id_      purchase_order_tab.vendor_no%TYPE;
   c_contract_         purchase_order_tab.contract%TYPE;
   
   CURSOR get_c_supplier IS
      SELECT vendor_no,contract
      FROM   purchase_order
      WHERE  objid      = objid_
      AND    objversion = objversion_;
   --(+)220914 TecPardhY BLK-IMM-29-M (FINISH)
   --(+)230817  TecJatinG    M-OS-094-M (START)
   CURSOR get_qty(order_no_ IN VARCHAR2, blanket_order_ IN VARCHAR2, blanket_line_ IN NUMBER) IS
      SELECT a.c_remaining_qty , a.objstate
      FROM   supp_blk_part a,
             purchase_order_line_tab b
      WHERE  b.blanket_order       = a.blanket_order
      AND    b.blanket_line        = a.blanket_line
      AND    b.part_no             = a.part_no
      AND    b.order_no            = order_no_
      AND    a.blanket_order       = blanket_order_
      AND    a.blanket_line        = blanket_line_
      AND    trunc(SYSDATE) BETWEEN trunc(a.c_valid_from) AND trunc(a.c_valid_to);
      
      CURSOR get_lines(order_no_ IN VARCHAR2) IS 
      SELECT pol.order_no,
             pol.line_no,
             pol.release_no,
             pol.blanket_order,
             pol.blanket_line,
             pol.demand_code,
             pol.order_code,
             pol.part_no
      FROM purchase_order_line_tab pol
      WHERE pol.order_no = order_no_;
   -- (+) 230817  TecJatinG    M-OS-094-M (FINISH)
BEGIN
   --(+)220914 TecPardhY BLK-IMM-29-M (START)
   OPEN  get_c_supplier;
   FETCH get_c_supplier INTO c_supplier_id_,c_contract_;
   CLOSE get_c_supplier;
  
   IF Supplier_Info_API.Get_C_Status_Db(c_supplier_id_) <> 'APPROVED' THEN
      Error_SYS.Record_General(lu_name_, 'CSUPERR: Supplier :P1 is not in approved state', c_supplier_id_);
   END IF;
   --(+)220914 TecPardhY BLK-IMM-29-M (FINISH)
    --(+) 230817  TecJatinG    M-OS-094-M (START)
   lu_rec_ := Get_Object_By_Id___(objid_);
   FOR i IN get_lines(lu_rec_.order_no) 
      LOOP
         c_qty_ := NULL;
         c_state_ := NULL;
         OPEN  get_qty(i.order_no,i.blanket_order, i.blanket_line);
         FETCH get_qty INTO c_qty_, c_state_;
         CLOSE get_qty; 
         IF (i.demand_code = 'Shop Order' AND c_state_ = 'Closed' ) THEN 
            Error_SYS.Record_General(lu_name_,'STATEERR: Agreement No.'||i.blanket_order||' and line no. '||i.blanket_line|| ' is in Closed state');
         END IF;
         IF (i.demand_code = 'Shop Order' AND c_state_ = 'Stopped' ) THEN 
            Error_SYS.Record_General(lu_name_,'STATEERR: Agreement No.'||i.blanket_order||' and line no. '||i.blanket_line|| ' is in Stopped state');
         END IF;
         IF (c_qty_ IS NULL AND i.blanket_order IS NOT NULL) THEN 
            Error_SYS.Record_General(lu_name_,'VALIDITYERR: Supplier Agreement is not Valid.');
         END IF;
      END LOOP;  
   --(+) 230817  TecJatinG    M-OS-094-M (FINISH)
   -- (+) 231011  TecRahulM  BLK-IMM-15-Ma (START)
   $IF Component_Cmod_SYS.INSTALLED $THEN 
   IF C_Supplier_Debar_API.Check_Supplier_Comp_Debar(c_supplier_id_,c_contract_) = 1 THEN
      Error_SYS.Record_General(lu_name_, 'CSUPERR: Supplier :P1 is Debarred/Cancel Registered', c_supplier_id_);
   END IF;
   $END
   -- (+) 231011  TecRahulM  BLK-IMM-15-Ma (FINISH)
   super(info_, objid_, objversion_, attr_, action_);
   --(+) 231121 TechSurenS MERGE-ENV-ECON-1 (START)
   IF (action_ = 'DO') THEN
      rec_  := Get_Object_By_Id___(objid_);
      var3_ := Purchase_Order_API.Change_Status5(rec_.order_no);

      IF rec_.payment_method = 'LC' THEN
         BEGIN
        SELECT LC_NUMBER
          INTO lcno_
          FROM LC_OTHER_DETAILS
         WHERE ORDER_NO LIKE rec_.order_no
           AND COMPANY LIKE SITE_API.GET_COMPANY(rec_.CONTRACT);
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               Error_SYS.Record_General(lu_name_,
                                        'LCNTOPEN: LC is not opened against this PO');
         END;
         stat_ := Lc_Header_API.Get_Intimation_Send(Site_API.GET_COMPANY(rec_.CONTRACT),
                                                    lcno_);
         IF stat_ = 'FALSE' THEN
            amt_   := Lc_Other_Details_API.Get_TOTAL_PO_AMOUNT(Site_API.GET_COMPANY(rec_.CONTRACT),
                                                               lcno_);
            lcamt_ := Lc_Header_API.get_amount(Site_API.GET_COMPANY(rec_.CONTRACT),
                                               lcno_);
            IF lcamt_ < amt_ THEN
               Error_SYS.Record_General(lu_name_,'LCAMT: LC amount is not sufficient for this PO');
            END IF;
         ELSE
            Error_SYS.Record_General(lu_name_,'LCMOD: LC modification is pending');
         END IF;
         bnk_ := Lc_Header_API.Get_Opening_Bank(Site_API.GET_COMPANY(rec_.CONTRACT),
                                                lcno_);
         IF bnk_ != rec_.NEGOTIATING_BANK THEN
            Error_SYS.Record_General(lu_name_,'BNK: Negotitating bank should be same as in the LC');
         END IF;

      END IF;

      FOR Rec1_ IN C2(rec_.order_no) LOOP
         supl_curr_code_ := Purchase_Part_Supplier_API.Get_Currency_Code(rec1_.contract,
                                                                         rec1_.part_no,
                                                                         Purchase_Order_API.get_Vendor_No(rec_.order_no));
         OPEN C1(rec_.order_no);
         FETCH C1
        INTO company_;
         CLOSE C1;

         supl_currency_rate_ := Currency_Rate_API.Get_Currency_Rate(company_,
                                                                    supl_curr_code_,
                                                                    '1',
                                                                    rec_.order_date);
         Currency_Amount_API.Calc_Currency_Amount(supl_currency_amount_,
                                                  company_,
                                                  supl_curr_code_,
                                                  supl_currency_rate_,
                                                  rec1_.BUY_UNIT_PRICE);

               END LOOP;
      IF (Transaction_SYS.Logical_Unit_Is_Installed('FaObject')) THEN
      SELECT COUNT(*)
        INTO cnt_
        FROM Fixed_Asset_Details_Tab
       WHERE order_no = rec_.order_no;

         IF (cnt_ > 0) THEN
            DECLARE
               CURSOR get_line_no IS
            SELECT line_no
              FROM Fixed_Asset_Details_Tab
             WHERE order_no = rec_.order_no;

            BEGIN
               FOR j IN get_line_no LOOP
                  OPEN C1(rec_.order_no);
                  FETCH C1
              INTO company_;
                  CLOSE C1;
                  Purchase_Order_Line_Part_API.FA_PO_Linking(rec_.order_no,j.line_no,company_);
               END LOOP;
            END;
         END IF;
      END IF;
      IF rec_.rowstate = 'Released' THEN
         FOR i IN GetReqNo(rec_.order_no) LOOP
            budget_category_ := NULL;
            fin_year_        := NULL;
            cost_center_     := NULL;
            company_         := NULL;
            OPEN exist_budget(i.REQUISITION_NO);
            FETCH exist_budget
          INTO budget_category_, fin_year_, cost_center_, company_;
            CLOSE exist_budget;
            IF budget_category_ IS NOT NULL THEN

               FOR j IN GetPO(i.REQUISITION_NO) LOOP
                  dbms_output.put_line('  REQUISITION_NO =' || i.REQUISITION_NO);
                  line_cost_         := 0;
                  this_pr_line_cost_ := 0;
                  IF (j.part_no IS NOT NULL) THEN
                     line_cost_         := nvl(Purchase_Order_Line_API.Get_Line_Total(j.order_no,
                                                                                      j.line_no,
                                                                                      j.release_no),
                                               0);
                     this_pr_line_cost_ := Purchase_Req_Line_API.Get_Line_Total(i.REQUISITION_NO,
                                                                                j.req_line,
                                                                                j.req_release);
                  ELSE
                     line_cost_         := nvl(Purchase_Order_Line_Nopart_API.Get_Line_Total(j.order_no,
                                                                                             j.line_no,
                                                                                             j.release_no),
                                               0);
                     this_pr_line_cost_ := Purchase_Req_Line_Nopart_API.Get_Line_Total(i.REQUISITION_NO,
                                                                                       j.req_line,
                                                                                       j.req_release);
                  END IF;
                  IF line_cost_ > 0 THEN

                     used_amount_ := nvl(Department_Budget_API.GetPRAmountConsumed(budget_category_,
                                                                                   fin_year_,
                                                                                   cost_center_,
                                                                                   company_),
                                         0) + nvl(Department_Budget_API.GetPoAmountConsumed(budget_category_,
                                                                                               fin_year_,
                                                                                               cost_center_,
                                                                                               company_),
                                                     0) + nvl(Department_Budget_API.GetSubConAmountConsumed(budget_category_,
                                                                                                            fin_year_,
                                                                                                            cost_center_,
                                                                                                            company_),
                                                              0);


                     dbms_output.put_line('  used_amount_ =' || used_amount_);
                     budgetamount_ := nvl(Department_Budget_API.Get_Req_Budget_Amt(budget_category_,
                                                                                   fin_year_,
                                                                                   cost_center_,
                                                                                   company_),
                                          0);
                     left_amount_  := budgetamount_ - nvl(used_amount_, 0);
                     budget_info_ := '[Company [' || Company_ || '], Category [' ||
                              budget_category_ || '], Fin-Year [' ||
                              fin_year_ || '], Cost Centre [' ||
                              cost_center_ || ']'; -- 140626 <ADTAIN> C_G1403638-1 Department Budget to be Checked at POPR Level Customization

                     IF budgetamount_ < nvl(used_amount_, 0) + nvl(line_cost_, 0) THEN
                        --                                 Error_Sys.Record_General(lu_name_,'BUDG1: Budget Amount Left Is Only : ('||left_amount_||') For Budget Detail (:P1) ',Budget_info_);
                        Error_SYS.Record_General(lu_name_,
                                                 'BUDGERR: Available Budget Amount Left (' ||
                                                 left_amount_ ||
                                                 ') is not sufficient For Budget Detail (:P1), Allocate Budget to Proceed ',
                                                 budget_info_);

                     END IF;

                     Pr_Budget_Details_API.BookAmount(i.REQUISITION_NO,
                                                      'PO',
                                                      line_cost_,
                                                      rec_.order_no);

                  END IF;

               END LOOP;
               Pr_Budget_Details_API.Refresh_Budget(i.REQUISITION_NO);
            END IF;
         END LOOP;
      END IF;
      --  170217  ANJRIN   <C_G1768297-1 GST Development> start
      IF SUPPLIER_API.Get_Category(rec_.vendor_no) = 'Internal' THEN
         acquisition_site_ := Supplier_API.Get_Acquisition_Site(rec_.vendor_no);
         PURCHASE_ORDER_TRANSFER_API.Validate_PO_for_GST(rec_.order_no,
                                                         rec_.contract,
                                                         rec_.vendor_no,
                                                         acquisition_site_ );
      ELSE
        PURCH_STRUC_INSERTION_API.Validate_GST_Tax(rec_.order_no);
      END IF; 
      --  170217  ANJRIN   <C_G1768297-1 GST Development> end
   END IF;
   --(+) 231121 TechSurenS MERGE-ENV-ECON-1 (FINISH)
END Release__;
--(+)220914 TecPardhY BLK-IMM-29-M (FINISH)
@Overtake Core
PROCEDURE Restrict_Po_Header_Update__ (
   newrec_             IN PURCHASE_ORDER_TAB%ROWTYPE,
   oldrec_             IN PURCHASE_ORDER_TAB%ROWTYPE,
   server_data_change_ IN NUMBER)
IS
BEGIN
$TEXTSEARCH
   (NVL(newrec_.addr_no,  string_null_)              != NVL(oldrec_.addr_no,  string_null_)) OR
$TEXTAPPEND
--(+)231210 TecFarahS BLK-IMM-31-M-HALP-8680(START)
 (NVL(newrec_.purchase_code,  string_null_)              != NVL(oldrec_.purchase_code,  string_null_)) OR
--(+)231210 TecFarahS BLK-IMM-31-M-HALP-8680(FINISH)
$TEXTEND   
END Restrict_Po_Header_Update__;

--(+)231114 TecVaruSa BLK-IMM-02-M (START)
@Override
PROCEDURE Close__ (
   info_       OUT           VARCHAR2,
   objid_      IN            VARCHAR2,
   objversion_ IN OUT NOCOPY VARCHAR2,
   attr_       IN OUT NOCOPY VARCHAR2,
   action_     IN            VARCHAR2 )
IS 
   rec_ purchase_order_tab%ROWTYPE;
BEGIN
   rec_ := Get_Object_By_Id___(objid_);
   C_Short_Close_Reason_Update(rec_.order_no,'','','Header');
   
   super(info_, objid_, objversion_, attr_, action_);
END Close__;
--(+)231114 TecVaruSa BLK-IMM-02-M (FINISH)
-------------------- LU SPECIFIC PROTECTED METHODS --------------------------


-------------------- LU SPECIFIC PUBLIC METHODS -----------------------------
--(+)230526 TecVaruSa M-OS-002_M-OS-053-M (Start)
--(+) 231121 TechSurenS MERGE-ENV-ECON-1 (FINISH) Override to Overtake
@Overtake Core
PROCEDURE User_Requisition_Line_To_Order (
   req_order_no_          IN OUT VARCHAR2,
   assg_line_no_          IN OUT VARCHAR2,
   assg_release_no_       IN OUT VARCHAR2,
   req_info_              IN OUT VARCHAR2,
   requisition_no_        IN     VARCHAR2,
   line_no_               IN     VARCHAR2,
   release_no_            IN     VARCHAR2,
   authorize_code_        IN     VARCHAR2,
   buyer_code_            IN     VARCHAR2,
   new_order_             IN     VARCHAR2 DEFAULT NULL,
   use_default_buyer_     IN     VARCHAR2 DEFAULT NULL,
   purchase_site_         IN     VARCHAR2 DEFAULT NULL,
   central_order_flag_    IN     VARCHAR2 DEFAULT 'NOT CENTRAL ORDER',
   check_approval_amount_ IN     VARCHAR2 DEFAULT 'TRUE',
   inquiry_no_            IN     NUMBER   DEFAULT NULL,
   inquiry_line_          IN     NUMBER   DEFAULT NULL,
   init_po_no_            IN     NUMBER   DEFAULT NULL, 
   prefix_                IN     VARCHAR2 DEFAULT NULL,
   orderqty_		        IN     NUMBER   DEFAULT NULL,
   suppliercontact_       IN     VARCHAR2 DEFAULT NULL )
IS
   --(+) 231121 TechSurenS MERGE-ENV-ECON-1 (START)
   $SEARCH
   route_id_                    VARCHAR2(12);
   $APPEND
-- India Localisation <Start>
   altpart_no_				         VARCHAR2(50);
   purch_unit_meas_            VARCHAR2(10);
   uom_conv_fact_			         NUMBER;
   req_curr_code_              VARCHAR2(3);
-- India Localisation <End>   
   deliv_type_id_              Purchase_Order_Line_Tab.Deliv_Type_Id%TYPE;
   hsn_sac_code_               Purchase_Order_Line_Tab.c_Hsn_Sac_Code%TYPE;
   unit_manual_base_amt_       Purchase_Order_Line_Tab.Unit_Manual_Base_Amount%TYPE;
   hsn_sac_type_db_            Purchase_Order_Line_Tab.c_Hsn_Sac_Type%TYPE;
   gst_applicable_db_          Purchase_Order_Line_Tab.c_Gst_Applicable%TYPE;
   gst_exemption_reason_       Purchase_Order_Line_Tab.c_Gst_Exemption_Reason%TYPE;
   pos_                        Purchase_Order_Line_Tab.c_Pos%TYPE; 
   --(+)230817  TecJatinG    M-OS-094-M (START)
   c_qty_               NUMBER;
   c_ord_qty_           NUMBER;
   c_conv_factor_       NUMBER;
   c_state_             VARCHAR2(32);
   CURSOR get_qty IS
      SELECT a.c_remaining_qty,a.objstate
      FROM   supp_blk_part a,
             purchase_req_line_part b
      WHERE  b.c_agreement_no      = a.blanket_order
      AND    b.c_agreement_line_no = a.blanket_line
      AND    b.part_no             = a.part_no
      AND    (b.c_qsb              = 'TRUE' 
             OR b.c_direct_loading = 'Yes')
      AND    b.requisition_no            = requisition_no_
      AND    trunc(SYSDATE) BETWEEN trunc(a.c_valid_from) AND trunc(a.c_valid_to)
      AND    b.demand_code         = 'Shop Order';
      
   CURSOR get_ord_qty(requisition_no_ IN VARCHAR2) IS
      SELECT SUM (original_qty),price_conv_factor
      FROM purchase_req_line_part
      WHERE (c_qsb              = 'TRUE' 
            OR c_direct_loading = 'Yes')
      AND requisition_no = requisition_no_
      GROUP BY price_conv_factor;
   CURSOR get_lines(requisition_no_ IN VARCHAR2) IS 
      SELECT prl.requisition_no,
             prl.line_no,
             prl.release_no,
             prl.c_agreement_no,
             prl.c_agreement_line_no,
             prl.demand_code,
             prl.order_code,
             prl.part_no
      FROM purchase_req_line_tab prl
      WHERE prl.requisition_no = requisition_no_;
   --(+)230817  TecJatinG    M-OS-094-M (FINISH)
   $END
   --(+) 231121 TechSurenS MERGE-ENV-ECON-1 (FINISH)
BEGIN
   
   $SEARCH
   Client_SYS.Clear_Attr(attr_);
                                   
   Purchase_Req_Line_API.Exist(requisition_no_, line_no_, release_no_);
   
   order_no_ := req_order_no_ ;
   $APPEND
   IF (Purchase_Req_Line_Part_API.Get_Demand_Code(requisition_no_, line_no_, release_no_) = 'Shop Order' AND 
      Purchase_Requisition_API.Get_Order_Code(requisition_no_) <> '0' AND 
      Purchase_Req_Line_Part_API.Get_C_Agreement_No(requisition_no_, line_no_, release_no_) IS NULL) THEN 
      Error_SYS.Record_General(lu_name_,'REQTOORDERERROR: Supplier Agreement Reference is Mandatory For Line Number :P1 and Release Number :P2 and Part Number :P3',line_no_,release_no_,Purchase_Req_Line_Part_API.Get_Part_No(requisition_no_, line_no_, release_no_));
   END IF;
   --(+) 230825  TecSurenS  M-BP-001(START)
   IF(Purchase_Requisition_API.Get_C_Budget_Linked_Db(requisition_no_) = 'FALSE')THEN
      Error_SYS.Record_General(lu_name_,'CBUDGETNOTLINKED: PO cannot be created when the budget is not linked to the requisition');
   END IF;
   --(+) 230825  TecSurenS  M-BP-001(FINISH)
   --(+) 230817  TecJatinG    M-OS-094-M (START)
   FOR i IN get_lines(requisition_no_) 
      LOOP
         c_qty_ := NULL;
         c_state_ := NULL;
         OPEN  get_qty;
         FETCH get_qty INTO c_qty_,c_state_;
         CLOSE get_qty;   
         OPEN  get_ord_qty(requisition_no_);
         FETCH get_ord_qty INTO c_ord_qty_,c_conv_factor_;
         CLOSE get_ord_qty;
         IF (i.demand_code = 'Shop Order' AND c_state_ = 'Closed' ) THEN 
            Error_SYS.Record_General(lu_name_,'STATEERR: Agreement No.'||i.c_agreement_no||' and line no. '||i.c_agreement_line_no|| ' is in Closed state');
         END IF;
         IF (i.demand_code = 'Shop Order' AND c_state_ = 'Stopped' ) THEN 
            Error_SYS.Record_General(lu_name_,'STATEERR: Agreement No.'||i.c_agreement_no||' and line no. '||i.c_agreement_line_no|| ' is in Stopped state');
         END IF;
         IF (c_qty_ IS NULL AND i.c_agreement_no IS NOT NULL) THEN 
            Error_SYS.Record_General(lu_name_,'VALIDITYERR: Supplier Agreement is not Valid.');
         END IF;
         IF (c_qty_ < c_ord_qty_/c_conv_factor_ AND i.c_agreement_no IS NOT NULL AND i.demand_code = 'Shop Order') THEN 
            Error_SYS.Record_General(lu_name_,'QTYERR: Adequate Supplier Blanket Quantity is not Available for Purchase Order No-'||req_order_no_ || ' Line No-' ||i.line_no|| ' Release No-' ||i.release_no || ' Part No-'|| i.part_no);
         END IF;
      END LOOP;  
    --(+) 230817  TecJatinG    M-OS-094-M (FINISH)
   $END
   
   --(+) 231121 TechSurenS MERGE-ENV-ECON-1 (START)
   $SEARCH
   site_date_                   := Site_API.Get_Site_Date(NVL(purchase_site_,contract_));
   $APPEND
   Purchase_Order_API.GPrefix_ := prefix_;
-- India Localisation <Start>
   IF GMultipleRow_ = 'TRUE' THEN
       Purchase_Order_API.GPoQty_  := REQUISITION_ORDER_DETAILS_API.Get_PR_Balance_Qty(requisition_no_,
                                        line_no_,
                                        release_no_,
                                        original_qty_);
	   
   ELSE
       Purchase_Order_API.GPoQty_  := orderqty_;
   END IF;
-- India Localisation <End>
   Purchase_Order_API.GPrefix_ := prefix_;
-- India Localisation <Start>
   IF GMultipleRow_ = 'TRUE' THEN
       get_req_pur_req_line_:= Purchase_Req_Line_API.Get (requisition_no_, line_no_, release_no_);

       Purchase_Order_API.GPoQty_  := REQUISITION_ORDER_DETAILS_API.Get_PR_Balance_Qty(requisition_no_,
                                        line_no_,
                                        release_no_,
                                        original_qty_);
   ELSE
       Purchase_Order_API.GPoQty_  := orderqty_;
   END IF;
-- India Localisation <End>
      altpart_no_ := Null;
-- India Localisation <Start>
-- if Purchase_Requisition_API.Get_Order_Code(requisition_no_) = '0' then
   IF PURCHASE_ORDER_TYPE_API.GET_TENDER_PROCESS(Purchase_Requisition_API.Get_Order_Code(requisition_no_)) = 'TRUE' THEN
      IF Purchase_Order_API.GDoc_No_ IS NOT NULL AND Purchase_Order_API.GTender_No_ IS NOT NULL THEN
          altpart_no_ := Tender_Bids_detail_api.Get_Alternate_PartNo (Purchase_Order_API.Gdoc_no_,Purchase_Order_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_ );
          IF altpart_no_ IS NOT NULL THEN
             part_no_ := altpart_no_; 
          END IF;
	    END IF;
   END IF;
-- India Localisation <End>      
   $END
   $PREPEND
   -- India Localisation <Start>
   IF Purchase_Order_API.GPoQty_ IS NOT NULL THEN
		   buy_qty_due_         := round(Purchase_Order_API.GPoQty_, NO_OF_DECIMALS );
   ELSE
		   buy_qty_due_         := round(original_qty_, NO_OF_DECIMALS );
   END IF;	
   -- India Localisation <End>
   $SEARCH
   wanted_receipt_date_ := get_req_pur_req_line_.wanted_receipt_date;
   $END
   $SEARCH
      Purchase_Order_Line_Util_API.Fetch_Create_Po_Line(sample_qty_,
   $REPLACE
         IF PURCHASE_ORDER_TYPE_API.GET_TENDER_PROCESS(Purchase_Requisition_API.Get_Order_Code(requisition_no_)) = 'TRUE' THEN
      	IF Purchase_Order_API.GDoc_No_ IS NOT NULL THEN
		       vendor_no_			  := Tender_Bids_api.Get_Vendor_no(Purchase_Order_API.GDoc_no_);
      	ELSE
	         vendor_no_       := Purchase_Req_Line_Part_API.Get_Vendor_No(requisition_no_, line_no_, release_no_);
	  	  END IF;
   	ELSE
      -- Note: Process Requisitions with parts
		    vendor_no_         := Purchase_Req_Line_Part_API.Get_Vendor_No(requisition_no_, line_no_, release_no_);
   	END IF;
	    --- Kishore Nenwani Tender Bug Start date 03/05/2005
      IF PURCHASE_ORDER_API.GDoc_No_ IS NOT NULL AND PURCHASE_ORDER_API.GTender_No_ IS NOT NULL THEN
      		ship_via_code_   := nvl(TENDER_BIDS_TERMS_API.Get_Final_Ship_Via_Code(PURCHASE_ORDER_API.GDoc_No_),TENDER_BIDS_TERMS_API.Get_Ship_Via_Code(PURCHASE_ORDER_API.GDoc_No_)) ;
      		delivery_terms_  := nvl(TENDER_BIDS_TERMS_API.Get_Final_Delivery_Terms(PURCHASE_ORDER_API.GDoc_No_),TENDER_BIDS_TERMS_API.Get_Delivery_Terms(PURCHASE_ORDER_API.GDoc_No_)) ;
      		pay_term_id_     := nvl(TENDER_BIDS_TERMS_API.Get_Final_Pay_Term_Id(PURCHASE_ORDER_API.GDoc_No_),TENDER_BIDS_TERMS_API.Get_Pay_Term_Id(PURCHASE_ORDER_API.GDoc_No_));
     		  Language_code_ := Supplier_API.Get_Language_Code(Vendor_no_);
   	END IF;
      Purchase_Order_Line_Util_API.Fetch_Create_Po_Line(sample_qty_,

   $END
      $SEARCH
      vendor_no_                := get_rec_pur_req_line_nopart_.vendor_no;
      $REPLACE
      IF PURCHASE_ORDER_TYPE_API.GET_TENDER_PROCESS(Purchase_Requisition_API.Get_Order_Code(requisition_no_)) = 'TRUE' THEN
         IF PURCHASE_ORDER_API.GDoc_No_ IS NOT NULL THEN
            vendor_no_			  := Tender_Bids_api.Get_Vendor_no(PURCHASE_ORDER_API.GDoc_no_);
         ELSE
            vendor_no_ := Purchase_Req_Line_NoPart_API.Get_Vendor_No( requisition_no_, line_no_, release_no_);
         END IF;
      ELSE
          vendor_no_              := get_rec_pur_req_line_nopart_.vendor_no;
      END IF;
      IF PURCHASE_ORDER_TYPE_API.GET_TENDER_PROCESS(Purchase_Requisition_API.Get_Order_Code(requisition_no_)) = 'TRUE' THEN
            IF PURCHASE_ORDER_API.GDoc_No_ IS NOT NULL AND PURCHASE_ORDER_API.GTender_No_ IS NOT NULL THEN
                 curr_code_ := Tender_Bids_detail_api.Get_CURRENCY_CODE (PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_ );
            END IF;
              Dbms_output.put_line ('Curr Code --> '||curr_code_);
      END IF;
      $END
   $SEARCH
   order_code_    := get_rec_pur_req_.order_code;
   $REPLACE
   order_code_ := Purchase_Requisition_API.Get_Order_Code(requisition_no_); 
   $END
   $SEARCH
   User_Proposal_To_Order(
      req_order_no_            => order_no_,
      assg_line_no_            => assg_line_no_,
      assg_release_no_         => assg_release_no_,
      req_info_                => info_,
      requisition_no_          => requisition_no_,
      line_no_                 => line_no_,
      release_no_              => release_no_,
      authorize_code_          => authorize_code_,
      buyer_code_              => t_buyer_code_,
      wanted_receipt_date_     => wanted_receipt_date_,
      contract_                => contract_,
      curr_code_               => curr_code_,
      delivery_address_        => delivery_address_,
      order_code_              => order_code_,
      language_code_           => language_code_,
      vendor_no_               => vendor_no_,
      blanket_order_           => blanket_order_,
      part_no_                 => part_no_,
      description_             => description_,
      buy_qty_due_             => buy_qty_due_,
      buy_unit_meas_           => buy_unit_meas_,
      unit_meas_               => unit_meas_,
      fbuy_unit_price_         => fbuy_unit_price_,
      fbuy_unit_price_incl_tax_ => fbuy_unit_price_incl_tax_,
      buy_unit_price_          => buy_unit_price_,
      buy_unit_price_incl_tax_ => buy_unit_price_incl_tax_,
      discount_                => discount_,
      close_code_              => close_code_,
      close_tolerance_         => close_tolerance_,
      inspection_code_         => inspection_code_,
      sample_qty_              => sample_qty_,
      sample_percent_          => sample_percent_,
      qc_code_                 => qc_code_,
      blanket_line_            => blanket_line_,
      additional_cost_amount_  => additional_cost_amount_,
      additional_cost_incl_tax_ => additional_cost_incl_tax_,
      price_conv_factor_       => price_conv_factor_,
      price_unit_meas_         => price_unit_meas_,
      curr_rate_               => curr_rate_,
      demand_code_             => demand_code_,
      note_id_                 => note_id_,
      note_                    => note_,
      pre_accounting_id_       => pre_accounting_id_,
      ship_via_code_           => ship_via_code_,
      delivery_terms_          => delivery_terms_,
      authorize_id_            => authorize_id_,
      pay_term_id_             => pay_term_id_,
      receive_case_            => receive_case_,
      fee_code_                => fee_code_,
      taxable_                 => taxable_,
      new_order_               => new_order_,
      assortment_              => assortment_,
      promised_delivery_date_  => promised_delivery_date_,
      stat_grp_                => stat_grp_,
      inquiry_no_              => inquiry_no_,
      inquiry_line_            => inquiry_line_,
      quote_revision_no_       => quote_revision_no_,
      condition_code_          => condition_code_,
      purchase_site_           => NVL(purchase_site_,contract_),
      price_freeze_            => price_freeze_,
      central_order_flag_      => central_order_flag_,      
      planned_poco_exist_      => planned_poco_exist_,
      chg_order_no_            => chg_order_no_,
      purchase_code_           => get_req_pur_req_line_.purchase_code,
      route_id_                => route_id_,
      vendor_part_no_          => vendor_part_no_,
      vendor_part_description_ => vendor_part_description_,
      tax_liability_type_db_   => tax_liability_type_db_,
      tax_calc_structure_id_   => tax_calc_structure_id_);
      $REPLACE
   IF PURCHASE_ORDER_TYPE_API.GET_TENDER_PROCESS(Purchase_Requisition_API.Get_Order_Code(requisition_no_)) = 'TRUE' THEN
      IF PURCHASE_ORDER_API.GDoc_No_ IS NOT NULL AND PURCHASE_ORDER_API.GTender_No_ IS NOT NULL THEN
	      purch_unit_meas_        := Tender_Bids_detail_api.Get_PURCH_UNIT_MEAS(PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_ );
         buy_unit_meas_		  := purch_unit_meas_;
         uom_conv_fact_          := Tender_Bids_detail_api.Get_PURCH_CONV_FACTOR(PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_ );
   	  IF NVL(uom_conv_fact_,0) = 0 THEN
		  	   uom_conv_fact_ := 1;
		  END IF;	
		  DBMS_OUTPUT.PUT_LINE('UOM PR/Tender/Conv. Factor :: '||buy_unit_meas_||'/'||purch_unit_meas_||'/'||To_char(uom_conv_fact_));
	      fbuy_unit_price_        := Tender_Bids_detail_api.Get_Basic_Price (PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_ );
		  DBMS_OUTPUT.PUT_LINE('Unit Price (From Tender Bids Details) :: '||fbuy_unit_price_);
		  IF NVL(uom_conv_fact_,0) = 0 THEN
             Error_SYS.Record_General(lu_name_, 'UOMERR: UOM Conv. Factor Should Not be Zero. Or Null');
		  END IF; 
		  DBMS_OUTPUT.PUT_LINE('PR Qty (Before Conv.) :: '||buy_qty_due_);
		  IF PURCHASE_ORDER_API.GPoQty_ IS NOT NULL THEN
---------- 		  buy_qty_due_			  := Round(PURCHASE_ORDER_API.GPoQty_ / uom_conv_fact_,3);
			  buy_qty_due_			  := Round(PURCHASE_ORDER_API.GPoQty_,3);
		  ELSE
			  buy_qty_due_			  := Round(original_qty_ / uom_conv_fact_,3);
		  END IF;	
		  DBMS_OUTPUT.PUT_LINE('PR Qty (After Conv.) :: '||buy_qty_due_);
    	  price_conv_factor_      := Tender_Bids_detail_api.Get_PRICE_CONV_FACTOR(PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_ );
	      price_unit_meas_        := Tender_Bids_detail_api.Get_PRICE_UNIT_MEAS(PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_ );
    	  wanted_receipt_date_    := Tender_Bids_detail_api.Get_DELIVERY_DATE (PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_ );
	      curr_code_              := Tender_Bids_detail_api.Get_CURRENCY_CODE (PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_ );
--	      curr_rate_              := Tender_Bids_detail_api.Get_CURRENCY_RATE (PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_ );
-- Karam for getting final_delivery_terms
        ship_via_code_          := nvl(TENDER_BIDS_TERMS_API.Get_Final_Ship_Via_Code(PURCHASE_ORDER_API.Gdoc_no_),TENDER_BIDS_TERMS_API.Get_Ship_Via_Code(PURCHASE_ORDER_API.Gdoc_no_));
        delivery_terms_          := nvl(TENDER_BIDS_TERMS_API.Get_Final_Delivery_Terms(PURCHASE_ORDER_API.Gdoc_no_),TENDER_BIDS_TERMS_API.Get_Delivery_Terms(PURCHASE_ORDER_API.Gdoc_no_));
        pay_term_id_          := nvl(TENDER_BIDS_TERMS_API.Get_Final_Pay_Term_Id(PURCHASE_ORDER_API.Gdoc_no_),TENDER_BIDS_TERMS_API.Get_Pay_Term_Id(PURCHASE_ORDER_API.Gdoc_no_));
-- End Karam

		  IF altpart_no_ IS NOT NULL THEN
			  description_ := PART_CATALOG_API.GET_DESCRIPTION(altpart_no_);
		  END IF;
		  	
		  IF PURCHASE_ORDER_API.GDelDate_ IS NOT NULL THEN
			   wanted_receipt_date_ := PURCHASE_ORDER_API.GDelDate_;
		  ELSE
			   wanted_receipt_date_ := get_req_pur_req_line_.Wanted_Receipt_Date;
		  END IF;	
        deliv_type_id_          := TENDER_BIDS_DETAIL_API.Get_Deliv_Type_Id(PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_);
        hsn_sac_code_           := TENDER_BIDS_DETAIL_API.Get_C_Hsn_Sac_Code(PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_);  
        unit_manual_base_amt_   := TENDER_BIDS_DETAIL_API.Get_Unit_Manual_Base_Amount(PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_);
        hsn_sac_type_db_        := TENDER_BIDS_DETAIL_API.Get_C_Hsn_Sac_Type_Db(PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_);
        gst_applicable_db_      := TENDER_BIDS_DETAIL_API.Get_C_Gst_Applicable_Db(PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_);
        gst_exemption_reason_   := TENDER_BIDS_DETAIL_API.Get_C_Gst_Exemption_Reason(PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_);
        pos_                    := TENDER_BIDS_DETAIL_API.Get_C_Pos(PURCHASE_ORDER_API.Gdoc_no_,PURCHASE_ORDER_API.Gtender_no_ ,requisition_no_ ,line_no_ ,release_no_);
	  END IF;
   END IF;
      
   User_Proposal_To_Order(
      req_order_no_            => order_no_,
      assg_line_no_            => assg_line_no_,
      assg_release_no_         => assg_release_no_,
      req_info_                => info_,
      requisition_no_          => requisition_no_,
      line_no_                 => line_no_,
      release_no_              => release_no_,
      authorize_code_          => authorize_code_,
      buyer_code_              => t_buyer_code_,
      wanted_receipt_date_     => wanted_receipt_date_,
      contract_                => contract_,
      curr_code_               => curr_code_,
      delivery_address_        => delivery_address_,
      order_code_              => order_code_,
      language_code_           => language_code_,
      vendor_no_               => vendor_no_,
      blanket_order_           => blanket_order_,
      part_no_                 => part_no_,
      description_             => description_,
      buy_qty_due_             => buy_qty_due_,
      buy_unit_meas_           => buy_unit_meas_,
      unit_meas_               => unit_meas_,
      fbuy_unit_price_         => fbuy_unit_price_,
      fbuy_unit_price_incl_tax_ => fbuy_unit_price_incl_tax_,
      buy_unit_price_          => buy_unit_price_,
      buy_unit_price_incl_tax_ => buy_unit_price_incl_tax_,
      discount_                => discount_,
      close_code_              => close_code_,
      close_tolerance_         => close_tolerance_,
      inspection_code_         => inspection_code_,
      sample_qty_              => sample_qty_,
      sample_percent_          => sample_percent_,
      qc_code_                 => qc_code_,
      blanket_line_            => blanket_line_,
      additional_cost_amount_  => additional_cost_amount_,
      additional_cost_incl_tax_ => additional_cost_incl_tax_,
      price_conv_factor_       => price_conv_factor_,
      price_unit_meas_         => price_unit_meas_,
      curr_rate_               => curr_rate_,
      demand_code_             => demand_code_,
      note_id_                 => note_id_,
      note_                    => note_,
      pre_accounting_id_       => pre_accounting_id_,
      ship_via_code_           => ship_via_code_,
      delivery_terms_          => delivery_terms_,
      authorize_id_            => authorize_id_,
      pay_term_id_             => pay_term_id_,
      receive_case_            => receive_case_,
      fee_code_                => fee_code_,
      taxable_                 => taxable_,
      new_order_               => new_order_,
      assortment_              => assortment_,
      promised_delivery_date_  => promised_delivery_date_,
      stat_grp_                => stat_grp_,
      inquiry_no_              => inquiry_no_,
      inquiry_line_            => inquiry_line_,
      quote_revision_no_       => quote_revision_no_,
      condition_code_          => condition_code_,
      purchase_site_           => NVL(purchase_site_,contract_),
      price_freeze_            => price_freeze_,
      central_order_flag_      => central_order_flag_,      
      planned_poco_exist_      => planned_poco_exist_,
      chg_order_no_            => chg_order_no_,
      purchase_code_           => get_req_pur_req_line_.purchase_code,
      route_id_                => route_id_,
      vendor_part_no_          => vendor_part_no_,
      vendor_part_description_ => vendor_part_description_,
      tax_liability_type_db_   => tax_liability_type_db_,
      tax_calc_structure_id_   => tax_calc_structure_id_,
      Deliv_Type_Id_           => Deliv_Type_Id_,
      c_hsn_sac_code_          => hsn_sac_code_,
      unit_manual_base_amount_ => unit_manual_base_amt_,
      c_gst_applicable_        => gst_applicable_db_,
      c_gst_exemption_reason_  => gst_exemption_reason_,
      c_hsn_sac_type_          => hsn_sac_type_db_,
      c_pos_                   => pos_);
      $END
	  --(+) 231121 TechSurenS MERGE-ENV-ECON-1 (FINISH)
END User_Requisition_Line_To_Order;
--(+)230526 TecVaruSa M-OS-002_M-OS-053-M (Finish)

--(+) 231121 TechSurenS MERGE-ENV-ECON-1 (START)
@Overtake Core 
PROCEDURE User_Proposal_To_Order (
   req_order_no_                 IN OUT VARCHAR2,
   assg_line_no_                 IN OUT VARCHAR2,
   assg_release_no_              IN OUT VARCHAR2,
   req_info_                     IN OUT VARCHAR2,
   requisition_no_               IN     VARCHAR2,
   line_no_                      IN     VARCHAR2,
   release_no_                   IN     VARCHAR2,
   authorize_code_               IN     VARCHAR2,
   buyer_code_                   IN     VARCHAR2,
   wanted_receipt_date_          IN     DATE,
   contract_                     IN     VARCHAR2,
   curr_code_                    IN     VARCHAR2,
   delivery_address_             IN     VARCHAR2,
   order_code_                   IN     VARCHAR2,
   language_code_                IN     VARCHAR2,
   vendor_no_                    IN     VARCHAR2,
   blanket_order_                IN     VARCHAR2,
   part_no_                      IN     VARCHAR2,
   description_                  IN     VARCHAR2,
   buy_qty_due_                  IN     NUMBER,
   buy_unit_meas_                IN     VARCHAR2,
   unit_meas_                    IN     VARCHAR2,
   fbuy_unit_price_              IN     NUMBER,
   fbuy_unit_price_incl_tax_     IN     NUMBER,
   buy_unit_price_               IN     NUMBER,
   buy_unit_price_incl_tax_      IN     NUMBER,
   discount_                     IN     NUMBER,
   close_code_                   IN     VARCHAR2,
   close_tolerance_              IN     NUMBER,
   inspection_code_              IN     VARCHAR2,
   sample_qty_                   IN     NUMBER,
   sample_percent_               IN     NUMBER,
   qc_code_                      IN     VARCHAR2,
   blanket_line_                 IN     NUMBER,
   additional_cost_amount_       IN     NUMBER,
   additional_cost_incl_tax_     IN     NUMBER,
   price_conv_factor_            IN     NUMBER,
   price_unit_meas_              IN     VARCHAR2,
   curr_rate_                    IN     NUMBER,
   demand_code_                  IN     VARCHAR2,
   note_id_                      IN     NUMBER,
   note_                         IN     VARCHAR2,
   pre_accounting_id_            IN     NUMBER,
   ship_via_code_                IN     VARCHAR2,
   delivery_terms_               IN     VARCHAR2,
   authorize_id_                 IN     VARCHAR2,
   pay_term_id_                  IN     VARCHAR2,
   receive_case_                 IN     VARCHAR2,
   fee_code_                     IN     VARCHAR2 DEFAULT NULL,
   taxable_                      IN     VARCHAR2 DEFAULT NULL,
   new_order_                    IN     VARCHAR2 DEFAULT NULL,
   assortment_                   IN     VARCHAR2 DEFAULT NULL,
   promised_delivery_date_       IN     DATE     DEFAULT NULL,
   stat_grp_                     IN     VARCHAR2 DEFAULT NULL,
   inquiry_no_                   IN     NUMBER   DEFAULT NULL,
   inquiry_line_                 IN     NUMBER   DEFAULT NULL,
   configuration_id_             IN     VARCHAR2 DEFAULT '*',
   job_id_                       IN     VARCHAR2 DEFAULT NULL,
   quote_revision_no_            IN     NUMBER   DEFAULT NULL,
   is_order_proposal_            IN     VARCHAR2 DEFAULT NULL,
   condition_code_               IN     VARCHAR2 DEFAULT NULL,
   dop_id_                       IN     VARCHAR2   DEFAULT NULL,
   dop_order_id_                 IN     NUMBER   DEFAULT NULL,
   purchase_site_                IN     VARCHAR2 DEFAULT NULL,
   price_freeze_                 IN     VARCHAR2 DEFAULT NULL,
   central_order_flag_           IN     VARCHAR2 DEFAULT 'NOT CENTRAL ORDER',
   activity_seq_                 IN     NUMBER   DEFAULT NULL,   
   planned_poco_exist_           IN     VARCHAR2 DEFAULT NULL,
   chg_order_no_                 IN     VARCHAR2 DEFAULT NULL,
   purchase_code_                IN     VARCHAR2 DEFAULT NULL,
   route_id_                     IN     VARCHAR2 DEFAULT NULL,
   vendor_part_no_               IN     VARCHAR2 DEFAULT NULL,
   vendor_part_description_      IN     VARCHAR2 DEFAULT NULL,
   tax_liability_type_db_        IN     VARCHAR2 DEFAULT NULL,
   tax_calc_structure_id_        IN     VARCHAR2 DEFAULT NULL,
   transfer_tax_from_qutoe_line_ IN     VARCHAR2 DEFAULT 'FALSE',
   suppliercontact_          IN     VARCHAR2 DEFAULT NULL,
   deliv_type_id_            IN     VARCHAR2 DEFAULT NULL,
   c_hsn_sac_code_           IN     VARCHAR2 DEFAULT NULL,
   unit_manual_base_amount_  IN     NUMBER   DEFAULT NULL,
   c_gst_applicable_         IN     VARCHAR2 DEFAULT NULL,
   c_gst_exemption_reason_   IN     VARCHAR2 DEFAULT NULL,
   c_hsn_sac_type_           IN     VARCHAR2 DEFAULT NULL,
   c_pos_                    IN     VARCHAR2 DEFAULT NULL )
IS
   --(+) 231121 TechSurenS MERGE-ENV-ECON-1 (START)
   $SEARCH
   contact_name_                  VARCHAR2(100);
   $APPEND
   org_po_ref_                    VARCHAR2(30);
   --ENVNAGANB 230925 <TENDER SPLIT> (FINISH)
   $END
   --(+) 231121 TechSurenS MERGE-ENV-ECON-1 (FINISH)
BEGIN
   $SEARCH
   tax_liability_ := Tax_Handling_Purch_Util_API.Get_Tax_Liability(company_, vendor_no_);
   $APPEND
   IF (Purchase_Req_Line_Part_API.Get_Demand_Code(pur_req_line_part_rec_.requisition_no, pur_req_line_part_rec_.line_no, pur_req_line_part_rec_.release_no) = 'Shop Order' AND 
      Purchase_Requisition_API.Get_Order_Code(pur_req_line_part_rec_.requisition_no) = '1') THEN
      upo_t_buy_unit_price_           := pur_req_line_part_rec_.buy_unit_price;
      upo_t_buy_unit_price_incl_tax_  := pur_req_line_part_rec_.buy_unit_price_incl_tax; 
      upo_t_fbuy_unit_price_          := pur_req_line_part_rec_.fbuy_unit_price;
      upo_t_fbuy_price_incl_tax_      := pur_req_line_part_rec_.fbuy_unit_price_incl_tax; 
      upo_discount_                   := pur_req_line_part_rec_.discount;
      upo_price_unit_meas_            := pur_req_line_part_rec_.price_unit_meas;
      upo_price_conv_factor_          := pur_req_line_part_rec_.price_conv_factor;
      upo_curr_code_                  := pur_req_line_part_rec_.currency_code;
      upo_blanket_order_              := pur_req_line_part_rec_.c_agreement_no;
      upo_blanket_line_               := pur_req_line_part_rec_.c_agreement_line_no;
      closest_part_assort_line_       := to_number(pur_req_line_part_rec_.C_Assort_Line_No);
   END IF;
   $END
   --(+) 231121 TechSurenS MERGE-ENV-ECON-1 (START)
$SEARCH
      IF (order_code_ != '5') THEN
         -- Note: The Order Code on the PO should not be updated to '1' when the PO is created from a Requision of Order Code '6'.
         IF (order_code_ != '6') THEN
            po_header_order_code_ := '1';
         END IF;
      ELSE
         po_header_order_code_ := '5';
      END IF;
      $REPLACE
      IF GTender_No_ IS NOT NULL THEN  
         po_header_order_code_ := Purchase_Requisition_API.Get_Order_Code(requisition_no_);
      ELSE  
         IF (order_code_ != '5') THEN
            -- Note: The Order Code on the PO should not be updated to '1' when the PO is created from a Requision of Order Code '6'.
            IF (order_code_ != '6') THEN
               po_header_order_code_ := '1';
            END IF;
         ELSE
            po_header_order_code_ := '5';
         END IF;
      END IF;
      $END
$SEARCH
         IF (purchase_site_ IS NOT NULL) THEN
            Client_SYS.Add_To_Attr('CONTRACT', purchase_site_, attr_);
         ELSE
            Client_SYS.Add_To_Attr('CONTRACT', contract_, attr_);
         END IF;
$REPLACE
        IF purchase_order_api.GTender_No_ IS NOT NULL THEN
           Client_SYS.Add_To_Attr('CONTRACT', purchase_requisition_api.Get_Contract(requisition_no_), attr_);
     	     address_       := Site_API.Get_Delivery_Address(purchase_requisition_api.Get_Contract(requisition_no_));
        ELSE
         IF (purchase_site_ IS NOT NULL) THEN
            Client_SYS.Add_To_Attr('CONTRACT', purchase_site_, attr_);
         ELSE
            Client_SYS.Add_To_Attr('CONTRACT', contract_, attr_);
         END IF;
        END IF;
$END
         $PREPEND
         IF suppliercontact_ IS NOT NULL THEN 
          Client_SYS.Add_To_Attr('CONTACT', suppliercontact_, attr_);
         END IF;
         IF purchase_order_api.GPrefix_ IS NOT NULL THEN
             Client_SYS.Add_To_Attr('PREFIX',purchase_order_api.GPrefix_, attr_);
         END IF;
         --081120<AjShIn><P_C Requirements Tender and SubContract><Start>
         Client_SYS.Add_To_Attr('CENTRAL_ORDER_FLAG_DB',central_order_flag_, attr_);
         --081120<AjShIn><P_C Requirements Tender and SubContract><Finish>
         $SEARCH
         New__(info_, objid_, objversion_, attr_, 'DO');
         $END
         $SEARCH
         Purchase_Order_Line_Part_API.New_Order_Line(info_                      => info_,
                                                     assg_line_no_              => assg_line_no_,
                                                     assg_release_no_           => assg_release_no_,
                                                     order_no_                  => order_no_,
                                                     part_no_                   => part_no_,
                                                     contract_                  => contract_,
                                                     description_               => description_,
                                                     buy_qty_due_               => buy_qty_due_,
                                                     buy_unit_meas_             => buy_unit_meas_,
                                                     unit_meas_                 => unit_meas_,
                                                     fbuy_unit_price_           => upo_t_fbuy_unit_price_,
                                                     fbuy_unit_price_incl_tax_  => upo_t_fbuy_price_incl_tax_,
                                                     buy_unit_price_            => upo_t_buy_unit_price_,
                                                     buy_unit_price_incl_tax_   => upo_t_buy_unit_price_incl_tax_,
                                                     discount_                  => upo_discount_,
                                                     planned_receipt_date_      => planned_receipt_date_,
                                                     close_code_                => close_code_,
                                                     close_tolerance_           => close_tolerance_,
                                                     receive_case_              => receive_case_,
                                                     inspection_code_           => inspection_code_,
                                                     sample_qty_                => sample_qty_,
                                                     sample_percent_            => sample_percent_,
                                                     qc_code_                   => qc_code_,
                                                     blanket_line_              => upo_blanket_line_,
                                                     blanket_order_             => upo_blanket_order_,
                                                     additional_cost_amount_    => upo_additional_cost_amount_,
                                                     additional_cost_incl_tax_  => upo_additional_cost_incl_tax_,
                                                     price_conv_factor_         => upo_price_conv_factor_,
                                                     price_unit_meas_           => upo_price_unit_meas_,
                                                     curr_rate_                 => upo_curr_rate_,
                                                     curr_code_                 => upo_curr_code_,
                                                     demand_code_               => demand_code_,
                                                     requisition_no_            => requisition_no_,
                                                     line_no_                   => line_no_,
                                                     release_no_                => release_no_,
                                                     note_id_                   => note_id_,
                                                     note_                      => note_,
                                                     pre_accounting_id_         => pre_accounting_id_,
                                                     process_type_              => process_type_,
                                                     activity_seq_              => temp_activity_seq_,
                                                     manufacturer_id_           => manufacturer_id_,
                                                     promised_delivery_date_    => promised_delivery_date_,
                                                     stat_grp_                  => stat_grp_,
                                                     tech_coordinator_id_       => tech_coordinator_id_,
                                                     configuration_id_          => configuration_id_,
                                                     job_id_                    => job_id_,
                                                     demand_order_ref1_         => demand_order_no_,
                                                     demand_order_ref2_         => demand_rel_no_,
                                                     demand_order_ref3_         => demand_line_no_,
                                                     demand_order_ref4_         => demand_line_item_no_,
                                                     service_type_              => service_type_,
                                                     serial_no_                 => serial_no_,
                                                     lot_batch_no_              => lot_batch_no_,
                                                     condition_code_            => condition_code_,
                                                     dop_id_                    => dop_id_,
                                                     dop_order_id_              => dop_order_id_,
                                                     qty_on_order_              => qty_on_order_,
                                                     part_Ownership_            => ownership_,
                                                     owner_                     => pur_req_line_part_rec_.owning_customer_no,
                                                     exchange_Item_             => exchange_item_,
                                                     core_deposit_base_         => core_deposit_base_,
                                                     core_deposit_curr_         => core_deposit_curr_,
                                                     default_addr_flag_         => default_addr_flag_,
                                                     addr_flag_                 => addr_flag_,
                                                     address_id_                => address_id_,
                                                     del_terms_                 => line_delivery_terms_,
                                                     ship_via_                  => line_ship_via_code_,
                                                     ext_transport_calendar_id_ => line_ext_transport_cal_id_,
                                                     route_id_                  => line_route_id_,
                                                     forwarder_id_              => line_forward_agent_id_,
                                                     price_freeze_              => price_freeze_,
                                                     inquiry_no_                => inquiry_no_,
                                                     internal_destination_      => destination_id_,
                                                     internal_dest_desc_        => internal_destination_,                                                     
                                                     del_terms_location_        => line_del_terms_location_,
                                                     kanban_receipt_location_   => kanban_receipt_location_,
                                                     srv_service_type_          => srv_service_type_, 
                                                     project_address_           => project_address_,
                                                     input_gtin_no_             => input_gtin_no_,
                                                     input_unit_meas_           => input_unit_meas_,
                                                     input_qty_                 => input_qty_,
                                                     input_conv_factor_         => input_conv_factor_,
                                                     destination_warehouse_id_  => pur_req_line_part_rec_.destination_warehouse_id,
                                                     rental_attr_               => rental_attr_,
                                                     create_fa_obj_db_          => create_fa_obj_db_,
                                                     fa_obj_per_unit_db_        => fa_obj_per_unit_db_,
                                                     closest_part_assort_line_  => closest_part_assort_line_,
                                                     agrmt_part_assort_line_    => agrmt_part_assort_line_,
                                                     vendor_part_no_            => vendor_part_no_,
                                                     vendor_part_description_   => vendor_part_description_,
                                                     tax_liability_type_db_     => tax_liability_type_db_,                                                     
                                                     transfer_tax_from_qutoe_line_ => transfer_tax_from_qutoe_line_,
                                                     inquiry_line_no_           => inquiry_line_,
                                                     inquiry_line_revision_no_  => quote_revision_no_,
                                                     packing_instruction_id_    => packing_instruction_id_,
                                                     demand_order_code_         => demand_order_code_);
            $REPLACE 
                     Purchase_Order_Line_Part_API.New_Order_Line(info_                      => info_,
                                                     assg_line_no_              => assg_line_no_,
                                                     assg_release_no_           => assg_release_no_,
                                                     order_no_                  => order_no_,
                                                     part_no_                   => part_no_,
                                                     contract_                  => contract_,
                                                     description_               => description_,
                                                     buy_qty_due_               => buy_qty_due_,
                                                     buy_unit_meas_             => buy_unit_meas_,
                                                     unit_meas_                 => unit_meas_,
                                                     fbuy_unit_price_           => upo_t_fbuy_unit_price_,
                                                     fbuy_unit_price_incl_tax_  => upo_t_fbuy_price_incl_tax_,
                                                     buy_unit_price_            => upo_t_buy_unit_price_,
                                                     buy_unit_price_incl_tax_   => upo_t_buy_unit_price_incl_tax_,
                                                     discount_                  => upo_discount_,
                                                     planned_receipt_date_      => planned_receipt_date_,
                                                     close_code_                => close_code_,
                                                     close_tolerance_           => close_tolerance_,
                                                     receive_case_              => receive_case_,
                                                     inspection_code_           => inspection_code_,
                                                     sample_qty_                => sample_qty_,
                                                     sample_percent_            => sample_percent_,
                                                     qc_code_                   => qc_code_,
                                                     blanket_line_              => upo_blanket_line_,
                                                     blanket_order_             => upo_blanket_order_,
                                                     additional_cost_amount_    => upo_additional_cost_amount_,
                                                     additional_cost_incl_tax_  => upo_additional_cost_incl_tax_,
                                                     price_conv_factor_         => upo_price_conv_factor_,
                                                     price_unit_meas_           => upo_price_unit_meas_,
                                                     curr_rate_                 => upo_curr_rate_,
                                                     curr_code_                 => upo_curr_code_,
                                                     demand_code_               => demand_code_,
                                                     requisition_no_            => requisition_no_,
                                                     line_no_                   => line_no_,
                                                     release_no_                => release_no_,
                                                     note_id_                   => note_id_,
                                                     note_                      => note_,
                                                     pre_accounting_id_         => pre_accounting_id_,
                                                     process_type_              => process_type_,
                                                     activity_seq_              => temp_activity_seq_,
                                                     manufacturer_id_           => manufacturer_id_,
                                                     promised_delivery_date_    => promised_delivery_date_,
                                                     stat_grp_                  => stat_grp_,
                                                     tech_coordinator_id_       => tech_coordinator_id_,
                                                     configuration_id_          => configuration_id_,
                                                     job_id_                    => job_id_,
                                                     demand_order_ref1_         => demand_order_no_,
                                                     demand_order_ref2_         => demand_rel_no_,
                                                     demand_order_ref3_         => demand_line_no_,
                                                     demand_order_ref4_         => demand_line_item_no_,
                                                     service_type_              => service_type_,
                                                     serial_no_                 => serial_no_,
                                                     lot_batch_no_              => lot_batch_no_,
                                                     condition_code_            => condition_code_,
                                                     dop_id_                    => dop_id_,
                                                     dop_order_id_              => dop_order_id_,
                                                     qty_on_order_              => qty_on_order_,
                                                     part_Ownership_            => ownership_,
                                                     owner_                     => pur_req_line_part_rec_.owning_customer_no,
                                                     exchange_Item_             => exchange_item_,
                                                     core_deposit_base_         => core_deposit_base_,
                                                     core_deposit_curr_         => core_deposit_curr_,
                                                     default_addr_flag_         => default_addr_flag_,
                                                     addr_flag_                 => addr_flag_,
                                                     address_id_                => address_id_,
                                                     del_terms_                 => line_delivery_terms_,
                                                     ship_via_                  => line_ship_via_code_,
                                                     ext_transport_calendar_id_ => line_ext_transport_cal_id_,
                                                     route_id_                  => line_route_id_,
                                                     forwarder_id_              => line_forward_agent_id_,
                                                     price_freeze_              => price_freeze_,
                                                     inquiry_no_                => inquiry_no_,
                                                     internal_destination_      => destination_id_,
                                                     internal_dest_desc_        => internal_destination_,                                                     
                                                     del_terms_location_        => line_del_terms_location_,
                                                     kanban_receipt_location_   => kanban_receipt_location_,
                                                     srv_service_type_          => srv_service_type_, 
                                                     project_address_           => project_address_,
                                                     input_gtin_no_             => input_gtin_no_,
                                                     input_unit_meas_           => input_unit_meas_,
                                                     input_qty_                 => input_qty_,
                                                     input_conv_factor_         => input_conv_factor_,
                                                     destination_warehouse_id_  => pur_req_line_part_rec_.destination_warehouse_id,
                                                     rental_attr_               => rental_attr_,
                                                     create_fa_obj_db_          => create_fa_obj_db_,
                                                     fa_obj_per_unit_db_        => fa_obj_per_unit_db_,
                                                     closest_part_assort_line_  => closest_part_assort_line_,
                                                     agrmt_part_assort_line_    => agrmt_part_assort_line_,
                                                     vendor_part_no_            => vendor_part_no_,
                                                     vendor_part_description_   => vendor_part_description_,
                                                     tax_liability_type_db_     => tax_liability_type_db_,                                                     
                                                     transfer_tax_from_qutoe_line_ => transfer_tax_from_qutoe_line_,
                                                     inquiry_line_no_           => inquiry_line_,
                                                     inquiry_line_revision_no_  => quote_revision_no_,
                                                     packing_instruction_id_    => packing_instruction_id_,
                                                     demand_order_code_         => demand_order_code_,
                                                     Deliv_Type_Id_             => deliv_type_id_,
                                                     c_hsn_sac_code_            => c_hsn_sac_code_,
                                                     unit_manual_base_amount_   => unit_manual_base_amount_,
                                                     c_gst_applicable_          => c_gst_applicable_,
                                                     c_gst_exemption_reason_    => c_gst_exemption_reason_,
                                                     c_hsn_sac_type_            => c_hsn_sac_type_,
                                                     c_pos_                     => c_pos_);
                  $END                                   
         $SEARCH
         Purch_Chg_Ord_Line_API.New_Change_Order_Line(info_                      => info_,
                                                      line_no_                   => assg_line_no_,
                                                      release_no_                => assg_release_no_,
                                                      order_no_                  => req_order_no_,
                                                      chg_order_no_              => chg_order_no_,
                                                      contract_                  => contract_,
                                                      description_               => description_,
                                                      buy_qty_due_               => buy_qty_due_,
                                                      buy_unit_meas_             => buy_unit_meas_,
                                                      fbuy_unit_price_           => upo_t_fbuy_unit_price_,
                                                      fbuy_unit_price_incl_tax_  => upo_t_fbuy_price_incl_tax_,
                                                      discount_                  => upo_discount_,
                                                      blanket_line_              => upo_blanket_line_,
                                                      blanket_order_             => upo_blanket_order_,
                                                      additional_cost_amount_    => upo_additional_cost_amount_,
                                                      additional_cost_incl_tax_  => upo_additional_cost_incl_tax_,
                                                      price_conv_factor_         => upo_price_conv_factor_,
                                                      curr_rate_                 => upo_curr_rate_,
                                                      requisition_no_            => requisition_no_,
                                                      req_line_no_               => line_no_,
                                                      req_release_no_            => release_no_,
                                                      stat_grp_                  => stat_grp_,
                                                      default_addr_flag_         => default_addr_flag_,
                                                      addr_flag_                 => addr_flag_,
                                                      address_id_                => address_id_,
                                                      delivery_terms_            => line_delivery_terms_,
                                                      ship_via_code_             => line_ship_via_code_,
                                                      ext_transport_calendar_id_ => line_ext_transport_cal_id_,
                                                      del_terms_location_        => line_del_terms_location_,
                                                      destination_id_            => destination_id_,
                                                      internal_destination_      => internal_destination_,
                                                      note_id_                   => note_id_,
                                                      note_text_                 => note_,
                                                      lu_name_                   => 'PurchChgOrdPartLine',
                                                      part_no_                   => part_no_,
                                                      condition_code_            => condition_code_,
                                                      ownership_                 => ownership_,
                                                      owner_                     => pur_req_line_part_rec_.owning_customer_no,
                                                      service_type_              => service_type_,
                                                      assortment_                => assortment_,
                                                      planned_receipt_date_      => planned_receipt_date_,
                                                      price_unit_meas_           => upo_price_unit_meas_,
                                                      taxable_                   => NULL,
                                                      activity_seq_              => temp_activity_seq_,
                                                      project_address_flag_      => project_address_,
                                                      route_id_                  => line_route_id_,
                                                      forwarder_id_              => line_forward_agent_id_,
                                                      price_freeze_              => price_freeze_,
                                                      destination_warehouse_id_  => pur_req_line_part_rec_.destination_warehouse_id,
                                                      closest_part_assort_line_  => closest_part_assort_line_,
                                                      agrmt_part_assort_line_    => agrmt_part_assort_line_,
                                                      vendor_part_no_            => vendor_part_no_,
                                                      vendor_part_description_   => vendor_part_description_,
                                                      serial_no_                 => serial_no_,
                                                      lot_batch_no_              => lot_batch_no_,
                                                      tax_liability_             => tax_liability_,
                                                      tax_liability_type_        => tax_liability_type_db_,
                                                      transfer_tax_from_qutoe_line_ => transfer_tax_from_qutoe_line_,
                                                      inquiry_no_                => inquiry_no_,
                                                      inquiry_line_no_           => inquiry_line_,
                                                      inquiry_line_revision_no_  => quote_revision_no_);
         $REPLACE
         Purch_Chg_Ord_Line_API.New_Change_Order_Line(info_                      => info_,
                                                      line_no_                   => assg_line_no_,
                                                      release_no_                => assg_release_no_,
                                                      order_no_                  => req_order_no_,
                                                      chg_order_no_              => chg_order_no_,
                                                      contract_                  => contract_,
                                                      description_               => description_,
                                                      buy_qty_due_               => buy_qty_due_,
                                                      buy_unit_meas_             => buy_unit_meas_,
                                                      fbuy_unit_price_           => upo_t_fbuy_unit_price_,
                                                      fbuy_unit_price_incl_tax_  => upo_t_fbuy_price_incl_tax_,
                                                      discount_                  => upo_discount_,
                                                      blanket_line_              => upo_blanket_line_,
                                                      blanket_order_             => upo_blanket_order_,
                                                      additional_cost_amount_    => upo_additional_cost_amount_,
                                                      additional_cost_incl_tax_  => upo_additional_cost_incl_tax_,
                                                      price_conv_factor_         => upo_price_conv_factor_,
                                                      curr_rate_                 => upo_curr_rate_,
                                                      requisition_no_            => requisition_no_,
                                                      req_line_no_               => line_no_,
                                                      req_release_no_            => release_no_,
                                                      stat_grp_                  => stat_grp_,
                                                      default_addr_flag_         => default_addr_flag_,
                                                      addr_flag_                 => addr_flag_,
                                                      address_id_                => address_id_,
                                                      delivery_terms_            => line_delivery_terms_,
                                                      ship_via_code_             => line_ship_via_code_,
                                                      ext_transport_calendar_id_ => line_ext_transport_cal_id_,
                                                      del_terms_location_        => line_del_terms_location_,
                                                      destination_id_            => destination_id_,
                                                      internal_destination_      => internal_destination_,
                                                      note_id_                   => note_id_,
                                                      note_text_                 => note_,
                                                      lu_name_                   => 'PurchChgOrdPartLine',
                                                      part_no_                   => part_no_,
                                                      condition_code_            => condition_code_,
                                                      ownership_                 => ownership_,
                                                      owner_                     => pur_req_line_part_rec_.owning_customer_no,
                                                      service_type_              => service_type_,
                                                      assortment_                => assortment_,
                                                      planned_receipt_date_      => planned_receipt_date_,
                                                      price_unit_meas_           => upo_price_unit_meas_,
                                                      taxable_                   => NULL,
                                                      activity_seq_              => temp_activity_seq_,
                                                      project_address_flag_      => project_address_,
                                                      route_id_                  => line_route_id_,
                                                      forwarder_id_              => line_forward_agent_id_,
                                                      price_freeze_              => price_freeze_,
                                                      destination_warehouse_id_  => pur_req_line_part_rec_.destination_warehouse_id,
                                                      closest_part_assort_line_  => closest_part_assort_line_,
                                                      agrmt_part_assort_line_    => agrmt_part_assort_line_,
                                                      vendor_part_no_            => vendor_part_no_,
                                                      vendor_part_description_   => vendor_part_description_,
                                                      serial_no_                 => serial_no_,
                                                      lot_batch_no_              => lot_batch_no_,
                                                      tax_liability_             => tax_liability_,
                                                      tax_liability_type_        => tax_liability_type_db_,
                                                      transfer_tax_from_qutoe_line_ => transfer_tax_from_qutoe_line_,
                                                      inquiry_no_                => inquiry_no_,
                                                      inquiry_line_no_           => inquiry_line_,
                                                      inquiry_line_revision_no_  => quote_revision_no_,
                                                      Deliv_Type_Id_             => deliv_type_id_,
                                                      c_hsn_sac_code_            => c_hsn_sac_code_,
                                                      unit_manual_base_amount_   => unit_manual_base_amount_,
                                                      c_gst_applicable_          => c_gst_applicable_,
                                                      c_gst_exemption_reason_    => c_gst_exemption_reason_,
                                                      c_hsn_sac_type_            => c_hsn_sac_type_,
                                                      c_pos_                     => c_pos_);
         $END      
         $SEARCH
         Purchase_Order_Line_Nopart_API.New_Order_Line(info_                      => info_,
                                                       assg_line_no_              => assg_line_no_,
                                                       assg_release_no_           => assg_release_no_,
                                                       order_no_                  => order_no_,
                                                       contract_                  => contract_,
                                                       description_               => description_,
                                                       buy_qty_due_               => buy_qty_due_,
                                                       buy_unit_meas_             => buy_unit_meas_,
                                                       fbuy_unit_price_           => t_fbuy_unit_price_,
                                                       fbuy_unit_price_incl_tax_  => t_fbuy_unit_price_incl_tax_,
                                                       buy_unit_price_            => t_buy_unit_price_,
                                                       buy_unit_price_incl_tax_   => t_buy_unit_price_incl_tax_,
                                                       discount_                  => upo_discount_,
                                                       planned_receipt_date_      => planned_receipt_date_,
                                                       close_code_                => close_code_,
                                                       close_tolerance_           => close_tolerance_,
                                                       blanket_line_              => upo_blanket_line_,
                                                       blanket_order_             => upo_blanket_order_,
                                                       additional_cost_amount_    => upo_additional_cost_amount_,
                                                       additional_cost_incl_tax_  => upo_additional_cost_incl_tax_,
                                                       price_conv_factor_         => price_conv_factor_,
                                                       price_unit_meas_           => price_unit_meas_,
                                                       currency_rate_             => t_curr_rate_,
                                                       currency_code_             => curr_code_,
                                                       demand_code_               => demand_code_,
                                                       requisition_no_            => requisition_no_,
                                                       line_no_                   => line_no_,
                                                       release_no_                => release_no_,
                                                       note_id_                   => note_id_,
                                                       note_                      => note_,
                                                       pre_accounting_id_         => pre_accounting_id_,
                                                       fee_code_                  => fee_code_,
                                                       taxable_                   => taxable_,
                                                       process_type_              => process_type_,
                                                       activity_seq_              => temp_activity_seq_,
                                                       assortment_                => assortment_,
                                                       promised_delivery_date_    => promised_delivery_date_,
                                                       stat_grp_                  => stat_grp_,
                                                       tech_coordinator_id_       => tech_coordinator_id_,
                                                       default_addr_flag_         => default_addr_flag_,
                                                       addr_flag_                 => addr_flag_,
                                                       address_id_                => address_id_,
                                                       del_terms_                 => line_delivery_terms_,
                                                       ship_via_                  => line_ship_via_code_,
                                                       ext_transport_calendar_id_ => line_ext_transport_cal_id_,
                                                       route_id_                  => line_route_id_,
                                                       forwarder_id_              => line_forward_agent_id_,
                                                       inquiry_no_                => inquiry_no_,
                                                       internal_destination_      => destination_id_,
                                                       internal_dest_desc_        => internal_destination_,                                                       
                                                       demand_order_ref1_         => demand_order_no_,
                                                       demand_order_ref2_         => demand_rel_no_,
                                                       demand_order_ref3_         => demand_line_no_,
                                                       demand_order_ref4_         => demand_line_item_no_,
                                                       del_terms_location_        => line_del_terms_location_,
                                                       project_address_           => project_address_,
                                                       create_fa_obj_db_          => create_fa_obj_db_,
                                                       fa_obj_per_unit_db_        => fa_obj_per_unit_db_,
                                                       vendor_part_no_            => vendor_part_no_,
                                                       vendor_part_description_   => vendor_part_description_, 
                                                       tax_liability_type_db_      => tax_liability_type_db_,
                                                       tax_calc_structure_id_     => tax_calc_structure_id_,
                                                       transfer_tax_from_qutoe_line_ => transfer_tax_from_qutoe_line_,
                                                       inquiry_line_no_           => inquiry_line_,
                                                       inquiry_line_revision_no_  => quote_revision_no_);
         $REPLACE
         Purchase_Order_Line_Nopart_API.New_Order_Line(info_                      => info_,
                                                       assg_line_no_              => assg_line_no_,
                                                       assg_release_no_           => assg_release_no_,
                                                       order_no_                  => order_no_,
                                                       contract_                  => contract_,
                                                       description_               => description_,
                                                       buy_qty_due_               => buy_qty_due_,
                                                       buy_unit_meas_             => buy_unit_meas_,
                                                       fbuy_unit_price_           => t_fbuy_unit_price_,
                                                       fbuy_unit_price_incl_tax_  => t_fbuy_unit_price_incl_tax_,
                                                       buy_unit_price_            => t_buy_unit_price_,
                                                       buy_unit_price_incl_tax_   => t_buy_unit_price_incl_tax_,
                                                       discount_                  => upo_discount_,
                                                       planned_receipt_date_      => planned_receipt_date_,
                                                       close_code_                => close_code_,
                                                       close_tolerance_           => close_tolerance_,
                                                       blanket_line_              => upo_blanket_line_,
                                                       blanket_order_             => upo_blanket_order_,
                                                       additional_cost_amount_    => upo_additional_cost_amount_,
                                                       additional_cost_incl_tax_  => upo_additional_cost_incl_tax_,
                                                       price_conv_factor_         => price_conv_factor_,
                                                       price_unit_meas_           => price_unit_meas_,
                                                       currency_rate_             => t_curr_rate_,
                                                       currency_code_             => curr_code_,
                                                       demand_code_               => demand_code_,
                                                       requisition_no_            => requisition_no_,
                                                       line_no_                   => line_no_,
                                                       release_no_                => release_no_,
                                                       note_id_                   => note_id_,
                                                       note_                      => note_,
                                                       pre_accounting_id_         => pre_accounting_id_,
                                                       fee_code_                  => fee_code_,
                                                       taxable_                   => taxable_,
                                                       process_type_              => process_type_,
                                                       activity_seq_              => temp_activity_seq_,
                                                       assortment_                => assortment_,
                                                       promised_delivery_date_    => promised_delivery_date_,
                                                       stat_grp_                  => stat_grp_,
                                                       tech_coordinator_id_       => tech_coordinator_id_,
                                                       default_addr_flag_         => default_addr_flag_,
                                                       addr_flag_                 => addr_flag_,
                                                       address_id_                => address_id_,
                                                       del_terms_                 => line_delivery_terms_,
                                                       ship_via_                  => line_ship_via_code_,
                                                       ext_transport_calendar_id_ => line_ext_transport_cal_id_,
                                                       route_id_                  => line_route_id_,
                                                       forwarder_id_              => line_forward_agent_id_,
                                                       inquiry_no_                => inquiry_no_,
                                                       internal_destination_      => destination_id_,
                                                       internal_dest_desc_        => internal_destination_,                                                       
                                                       demand_order_ref1_         => demand_order_no_,
                                                       demand_order_ref2_         => demand_rel_no_,
                                                       demand_order_ref3_         => demand_line_no_,
                                                       demand_order_ref4_         => demand_line_item_no_,
                                                       del_terms_location_        => line_del_terms_location_,
                                                       project_address_           => project_address_,
                                                       create_fa_obj_db_          => create_fa_obj_db_,
                                                       fa_obj_per_unit_db_        => fa_obj_per_unit_db_,
                                                       vendor_part_no_            => vendor_part_no_,
                                                       vendor_part_description_   => vendor_part_description_, 
                                                       tax_liability_type_db_      => tax_liability_type_db_,
                                                       tax_calc_structure_id_     => tax_calc_structure_id_,
                                                       transfer_tax_from_qutoe_line_ => transfer_tax_from_qutoe_line_,
                                                       inquiry_line_no_           => inquiry_line_,
                                                       inquiry_line_revision_no_  => quote_revision_no_,
                                                       Deliv_Type_Id_             => deliv_type_id_,
                                                       c_hsn_sac_code_            => c_hsn_sac_code_,
                                                       unit_manual_base_amount_   => unit_manual_base_amount_,
                                                       c_gst_applicable_          => c_gst_applicable_,
                                                       c_gst_exemption_reason_    => c_gst_exemption_reason_,
                                                       c_hsn_sac_type_            => c_hsn_sac_type_,
                                                       c_pos_                     => c_pos_);
         $END                                     
         $SEARCH         
         Purch_Chg_Ord_Line_API.New_Change_Order_Line(info_                      => info_,
                                                      line_no_                   => assg_line_no_,
                                                      release_no_                => assg_release_no_,
                                                      order_no_                  => req_order_no_,
                                                      chg_order_no_              => chg_order_no_,
                                                      contract_                  => contract_,
                                                      description_               => description_,
                                                      buy_qty_due_               => buy_qty_due_,
                                                      buy_unit_meas_             => buy_unit_meas_,
                                                      fbuy_unit_price_           => t_fbuy_unit_price_,
                                                      fbuy_unit_price_incl_tax_  => t_fbuy_unit_price_incl_tax_,
                                                      discount_                  => upo_discount_,
                                                      blanket_line_              => upo_blanket_line_,
                                                      blanket_order_             => upo_blanket_order_,
                                                      additional_cost_amount_    => upo_additional_cost_amount_,
                                                      additional_cost_incl_tax_  => upo_additional_cost_incl_tax_,
                                                      price_conv_factor_         => price_conv_factor_,
                                                      curr_rate_                 => t_curr_rate_,
                                                      requisition_no_            => requisition_no_,
                                                      req_line_no_               => line_no_,
                                                      req_release_no_            => release_no_,
                                                      stat_grp_                  => stat_grp_,
                                                      default_addr_flag_         => default_addr_flag_,
                                                      addr_flag_                 => addr_flag_,
                                                      address_id_                => address_id_,
                                                      delivery_terms_            => line_delivery_terms_,
                                                      ship_via_code_             => line_ship_via_code_,
                                                      ext_transport_calendar_id_ => line_ext_transport_cal_id_,
                                                      del_terms_location_        => line_del_terms_location_,
                                                      destination_id_            => destination_id_,
                                                      internal_destination_      => internal_destination_,
                                                      note_id_                   => note_id_,
                                                      note_text_                 => note_,
                                                      lu_name_                   => 'PurchChgOrdNopartLine',
                                                      part_no_                   => NULL,
                                                      condition_code_            => NULL,
                                                      ownership_                 => NULL,
                                                      owner_                     => NULL,
                                                      service_type_              => NULL,
                                                      assortment_                => assortment_,
                                                      planned_receipt_date_      => planned_receipt_date_,
                                                      price_unit_meas_           => NULL,
                                                      taxable_                   => taxable_,
                                                      activity_seq_              => temp_activity_seq_,
                                                      project_address_flag_      => project_address_,
                                                      route_id_                  => line_route_id_,
                                                      forwarder_id_              => line_forward_agent_id_,
                                                      price_freeze_              => NULL,
                                                      closest_part_assort_line_  => closest_part_assort_line_,
                                                      agrmt_part_assort_line_    => agrmt_part_assort_line_,
                                                      vendor_part_no_            => vendor_part_no_,
                                                      vendor_part_description_   => vendor_part_description_,
                                                      serial_no_                 => serial_no_,
                                                      lot_batch_no_              => lot_batch_no_,
                                                      tax_liability_             => tax_liability_,
                                                      tax_liability_type_        => tax_liability_type_db_,
                                                      fee_code_                  => fee_code_,
                                                      tax_calc_structure_id_     => tax_calc_structure_id_,
                                                      transfer_tax_from_qutoe_line_ => transfer_tax_from_qutoe_line_,
                                                      inquiry_no_                => inquiry_no_,
                                                      inquiry_line_no_           => inquiry_line_,
                                                      inquiry_line_revision_no_  => quote_revision_no_);
            $REPLACE
         Purch_Chg_Ord_Line_API.New_Change_Order_Line(info_                      => info_,
                                                      line_no_                   => assg_line_no_,
                                                      release_no_                => assg_release_no_,
                                                      order_no_                  => req_order_no_,
                                                      chg_order_no_              => chg_order_no_,
                                                      contract_                  => contract_,
                                                      description_               => description_,
                                                      buy_qty_due_               => buy_qty_due_,
                                                      buy_unit_meas_             => buy_unit_meas_,
                                                      fbuy_unit_price_           => t_fbuy_unit_price_,
                                                      fbuy_unit_price_incl_tax_  => t_fbuy_unit_price_incl_tax_,
                                                      discount_                  => upo_discount_,
                                                      blanket_line_              => upo_blanket_line_,
                                                      blanket_order_             => upo_blanket_order_,
                                                      additional_cost_amount_    => upo_additional_cost_amount_,
                                                      additional_cost_incl_tax_  => upo_additional_cost_incl_tax_,
                                                      price_conv_factor_         => price_conv_factor_,
                                                      curr_rate_                 => t_curr_rate_,
                                                      requisition_no_            => requisition_no_,
                                                      req_line_no_               => line_no_,
                                                      req_release_no_            => release_no_,
                                                      stat_grp_                  => stat_grp_,
                                                      default_addr_flag_         => default_addr_flag_,
                                                      addr_flag_                 => addr_flag_,
                                                      address_id_                => address_id_,
                                                      delivery_terms_            => line_delivery_terms_,
                                                      ship_via_code_             => line_ship_via_code_,
                                                      ext_transport_calendar_id_ => line_ext_transport_cal_id_,
                                                      del_terms_location_        => line_del_terms_location_,
                                                      destination_id_            => destination_id_,
                                                      internal_destination_      => internal_destination_,
                                                      note_id_                   => note_id_,
                                                      note_text_                 => note_,
                                                      lu_name_                   => 'PurchChgOrdNopartLine',
                                                      part_no_                   => NULL,
                                                      condition_code_            => NULL,
                                                      ownership_                 => NULL,
                                                      owner_                     => NULL,
                                                      service_type_              => NULL,
                                                      assortment_                => assortment_,
                                                      planned_receipt_date_      => planned_receipt_date_,
                                                      price_unit_meas_           => NULL,
                                                      taxable_                   => taxable_,
                                                      activity_seq_              => temp_activity_seq_,
                                                      project_address_flag_      => project_address_,
                                                      route_id_                  => line_route_id_,
                                                      forwarder_id_              => line_forward_agent_id_,
                                                      price_freeze_              => NULL,
                                                      closest_part_assort_line_  => closest_part_assort_line_,
                                                      agrmt_part_assort_line_    => agrmt_part_assort_line_,
                                                      vendor_part_no_            => vendor_part_no_,
                                                      vendor_part_description_   => vendor_part_description_,
                                                      serial_no_                 => serial_no_,
                                                      lot_batch_no_              => lot_batch_no_,
                                                      tax_liability_             => tax_liability_,
                                                      tax_liability_type_        => tax_liability_type_db_,
                                                      fee_code_                  => fee_code_,
                                                      tax_calc_structure_id_     => tax_calc_structure_id_,
                                                      transfer_tax_from_qutoe_line_ => transfer_tax_from_qutoe_line_,
                                                      inquiry_no_                => inquiry_no_,
                                                      inquiry_line_no_           => inquiry_line_,
                                                      inquiry_line_revision_no_  => quote_revision_no_,
                                                      Deliv_Type_Id_             => deliv_type_id_,
                                                      c_hsn_sac_code_            => c_hsn_sac_code_,
                                                      unit_manual_base_amount_   => unit_manual_base_amount_,
                                                      c_gst_applicable_          => c_gst_applicable_,
                                                      c_gst_exemption_reason_    => c_gst_exemption_reason_,
                                                      c_hsn_sac_type_            => c_hsn_sac_type_,
                                                      c_pos_                     => c_pos_);
            $END                  
			--(+) 231121 TechSurenS MERGE-ENV-ECON-1 (FINISH)
END User_Proposal_To_Order;

--(+) 231121 TechSurenS MERGE-ENV-ECON-1 (START)
@Override
PROCEDURE Create_Int_Purch_Order (
   po_order_no_            IN OUT VARCHAR2,
   po_line_no_             IN OUT VARCHAR2,
   po_rel_no_              IN OUT VARCHAR2,
   supply_code_            IN     VARCHAR2,
   vendor_no_              IN     VARCHAR2,
   part_no_                IN     VARCHAR2,
   contract_               IN     VARCHAR2,
   authorize_code_         IN     VARCHAR2,
   wanted_receipt_date_    IN     DATE,
   qty_on_order_           IN     NUMBER,
   customer_po_no_         IN     VARCHAR2,
   addr_1_                 IN     VARCHAR2,
   address1_               IN     VARCHAR2,
   address2_               IN     VARCHAR2,
   address3_               IN     VARCHAR2,
   address4_               IN     VARCHAR2,
   address5_               IN     VARCHAR2,
   address6_               IN     VARCHAR2,
   zip_code_               IN     VARCHAR2,
   city_                   IN     VARCHAR2,
   addr_state_             IN     VARCHAR2,
   county_                 IN     VARCHAR2,
   country_code_           IN     VARCHAR2,
   demand_order_ref1_      IN     VARCHAR2 DEFAULT NULL,
   demand_order_ref2_      IN     VARCHAR2 DEFAULT NULL,
   demand_order_ref3_      IN     VARCHAR2 DEFAULT NULL,
   demand_order_ref4_      IN     VARCHAR2 DEFAULT NULL,
   configuration_id_       IN     VARCHAR2 DEFAULT '*',
   condition_code_         IN     VARCHAR2 DEFAULT NULL,
   header_supply_code_     IN     VARCHAR2 DEFAULT NULL,
   deliver_to_customer_no_ IN     VARCHAR2 DEFAULT NULL,
   ean_location_del_addr_  IN     VARCHAR2 DEFAULT NULL,
   intrastat_exempt_       IN     VARCHAR2 DEFAULT NULL,
   activity_seq_           IN     NUMBER   DEFAULT NULL,
   rental_attr_            IN     VARCHAR2 DEFAULT NULL,
   packing_instruction_id_ IN     VARCHAR2 DEFAULT NULL,
    c_hsn_sac_code_         IN     VARCHAR2 DEFAULT NULL,
      unit_manual_base_amount_ IN    NUMBER   DEFAULT NULL,
      c_gst_applicable_       IN     VARCHAR2 DEFAULT NULL,
      c_gst_exemption_reason_ IN     VARCHAR2 DEFAULT NULL,
      c_pos_                  IN     VARCHAR2 DEFAULT NULL )
   
IS
         attr_                      VARCHAR2(32000);

BEGIN
   
   --Add pre-processing code here
   super(po_order_no_, po_line_no_, po_rel_no_, supply_code_, vendor_no_, part_no_, contract_, authorize_code_, wanted_receipt_date_, qty_on_order_, customer_po_no_, addr_1_, address1_, address2_, address3_, address4_, address5_, address6_, zip_code_, city_, addr_state_, county_, country_code_, demand_order_ref1_, demand_order_ref2_, demand_order_ref3_, demand_order_ref4_, configuration_id_, condition_code_, header_supply_code_, deliver_to_customer_no_, ean_location_del_addr_, intrastat_exempt_, activity_seq_, rental_attr_, packing_instruction_id_);
   --Add post-processing code here
    Client_SYS.Clear_Attr(attr_);
   --Client_SYS.Add_To_Attr('ORG_PO_REF', org_po_ref_, attr_);
   IF Nvl(Company_Tax_Info_Api.Get_Inv_Purch_Flag(Site_Api.Get_Company(contract_)),'FALSE') = 'TRUE' THEN
    --     Client_SYS.Add_To_Attr('DELIV_TYPE_ID',           deliv_type_id_,            attr_);
         Client_SYS.Add_To_Attr('C_HSN_SAC_CODE',          c_hsn_sac_code_,           attr_);
         Client_SYS.Add_To_Attr('UNIT_MANUAL_BASE_AMOUNT', unit_manual_base_amount_,  attr_);
         Client_SYS.Add_To_Attr('C_GST_APPLICABLE_DB',     c_gst_applicable_,         attr_);
         Client_SYS.Add_To_Attr('C_GST_EXEMPTION_REASON',  c_gst_exemption_reason_,   attr_);
   --      Client_SYS.Add_To_Attr('C_HSN_SAC_TYPE_DB',       c_hsn_sac_type_,           attr_);
         Client_SYS.Add_To_Attr('C_POS',                   c_pos_,                    attr_);
   END IF;
   Purchase_Order_Line_API.Modify_Line(po_order_no_, po_line_no_, po_rel_no_, attr_);
END Create_Int_Purch_Order;




@Override
PROCEDURE Cancel__ (
   info_       OUT    VARCHAR2,
   objid_      IN     VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   attr_       IN OUT VARCHAR2,
   action_     IN     VARCHAR2 )
IS
   rec_ purchase_order_tab%ROWTYPE;
   cnt_             NUMBER;
   count_           NUMBER;
   demand_order_no_ VARCHAR2(12);
   CURSOR get_insourcing_po_line_cnt(order_no_ IN VARCHAR2) IS 
      SELECT count(*)
      FROM   purchase_order_line_tab po
      WHERE  po.order_no = order_no_
      AND    po.c_insoucing_cons_hours > 0; 
      
   CURSOR get_demand_order_no(order_no_ IN VARCHAR2) IS 
      SELECT pol.demand_order_no
      FROM   purchase_order_line_tab pol
      WHERE  pol.order_no = order_no_
      AND    pol.c_insoucing_cons_hours > 0; 
BEGIN
   IF (action_ = 'DO') THEN
      rec_ := Get_Object_By_Id___(objid_);
      OPEN  get_demand_order_no(rec_.order_no);
      FETCH get_demand_order_no INTO count_;
      CLOSE get_demand_order_no;
      
      OPEN  get_insourcing_po_line_cnt(rec_.order_no);
      FETCH get_insourcing_po_line_cnt INTO demand_order_no_;
      CLOSE get_insourcing_po_line_cnt;
      
      IF count_ > 0 THEN 
         Error_SYS.Record_General(lu_name_, 'CPOCONSHOURERR: Cannot Cancel - Operation Hours already Reported in Shop Order No. :P1',demand_order_no_);
      END IF;
      $IF Component_Payled_SYS.INSTALLED $THEN
         SELECT Count(*) INTO cnt_
         FROM   Supplier_Payment_Schedule
         WHERE  Company  = Site_Api.Get_Company(rec_.Contract)
         AND    REF_No   = rec_.Order_No
         AND    STATE    != 'Cancelled'
         AND    SUPPLIER_PAYMENT_TYPE = 'AdvanceAgainstPo';
      $ELSE
         NULL;
      $END
      IF cnt_ > 0 THEN
         Error_SYS.Record_General('lu_name_','PAYEXISTERR: Cancellation Denied,Because Supplier Payment is Exist in Created State'); 
      END IF;
		IF purchase_order_approval_api.any_lines_approved(rec_.Order_No,'*')='TRUE' THEN
			Error_SYS.Record_General('lu_name_','PAYEXISTERR: Cancellation Not Allowed. The Purchase Order Is Authorized.');
		END IF;

   END IF;
   super(info_, objid_, objversion_, attr_, action_);
   IF (action_ = 'DO') THEN
      rec_ := Get_Object_By_Id___(objid_);
      Pr_Budget_Details_API.Refresh_PO_Amount(rec_.order_no,NULL,NULL);
   END IF;
END Cancel__;


@Override
PROCEDURE Confirm__ (
   info_       OUT    VARCHAR2,
   objid_      IN     VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   attr_       IN OUT VARCHAR2,
   action_     IN     VARCHAR2 )
IS

   company_  VARCHAR2(20);
   cnt_      NUMBER;

   CURSOR C1(order_no_  IN VARCHAR2) IS
    SELECT Company
    FROM Purchase_Order
    WHERE Order_No = order_no_;
   
   rec_     purchase_order_tab%ROWTYPE;
BEGIN
   IF (action_ = 'DO') THEN
      rec_ := Get_Object_By_Id___(objid_);
      IF (rec_.ROWSTATE != 'Released') AND (Transaction_SYS.Logical_Unit_Is_Installed('FaObject')) THEN
         SELECT COUNT(*) INTO cnt_
         FROM Fixed_Asset_Details_Tab
         WHERE order_no = rec_.order_no;

         IF (cnt_ > 0) THEN
            DECLARE
               CURSOR get_line_no IS
             SELECT line_no FROM Fixed_Asset_Details_Tab
             WHERE order_no = rec_.order_no;

            BEGIN
               FOR j IN get_line_no 
               LOOP
                  OPEN C1(rec_.order_no);
                  FETCH C1 INTO company_;
                  CLOSE C1;
                  Purchase_Order_Line_Part_API.FA_PO_Linking(rec_.order_no,j.line_no,company_);
               END LOOP;
            END;
         END IF;           
      END IF;
   END IF;
   super(info_, objid_, objversion_, attr_, action_);
END Confirm__;

@Override
PROCEDURE Insert___ (
   objid_      OUT    VARCHAR2,
   objversion_ OUT    VARCHAR2,
   newrec_     IN OUT PURCHASE_ORDER_TAB%ROWTYPE,
   attr_       IN OUT VARCHAR2 )
IS
  -- by anil
  prefix_no_   NUMBER;

  CURSOR Get_Details IS
    SELECT inv_purch_flag
      FROM COMPANY_TAX_INFO
     WHERE company = site_api.get_company(newrec_.contract);

  inv_info_       VARCHAR2(2000);
  inv_attr_       VARCHAR2(2000);
  inv_objid_      VARCHAR2(2000);
  inv_objversion_ VARCHAR2(2000);
  inv_purch_flag_ VARCHAR2(10);
BEGIN
  -- start aded by ajit
  prefix_ := Client_Sys.Get_Item_Value('PREFIX',attr_);
  IF newrec_.ORDER_NO IS NULL THEN
    IF Company_Tax_Info_API.get_purch_prefix_flag(Site_API.get_company(newrec_.contract)) = 'TRUE' THEN
      IF nvl(prefix_, 'x') = 'x' THEN
        prefix_ := Site_Prefix_API.get_default_prefix(Site_Site_Group_API.get_site_group(newrec_.contract,'PO'),'PO');
      END IF;
      Site_Prefix_API.valid_prefix_contract(newrec_.contract, prefix_,'PO');
      Site_Prefix_API.Exist(prefix_);
      Site_Prefix_API.Valid_Prefix(prefix_, newrec_.order_date, 'PO');
      Site_Prefix_API.Get_Next_No(prefix_, prefix_no_);
      newrec_.order_no := prefix_ || prefix_no_;
      Prefix_Transaction_API.ins_prefix_txn(newrec_.order_no,'PO', prefix_, prefix_ || prefix_no_);
    END IF;
  END IF;
  --ended by ajit
   super(objid_, objversion_, newrec_, attr_);
  inv_purch_flag_ := NULL;

  OPEN Get_Details;
  FETCH Get_Details
    INTO inv_purch_flag_;
  CLOSE Get_Details;

  inv_info_ := NULL;
  Client_SYS.Clear_Attr(inv_attr_);
  Client_SYS.Add_To_Attr('ORDER_NO', newrec_.order_no, inv_attr_);
  Client_SYS.Add_To_Attr('INV_PURCH_FLAG', nvl(inv_purch_flag_, 'FALSE'), inv_attr_);
  Client_SYS.Add_To_Attr('PAY_TERM_INST', 'Inv Entry Date', inv_attr_);
  Purchase_Order_Ind_API.New__(inv_info_,inv_objid_, inv_objversion_, inv_attr_,'DO');
END Insert___;
--(+) 231121 TechSurenS MERGE-ENV-ECON-1 (FINISH)

@Overtake Core
PROCEDURE New_Transfer_Order (
   order_no_  OUT VARCHAR2,
   contract_  IN  VARCHAR2,
   vendor_no_ IN  VARCHAR2,
   CAutoClose_ IN VARCHAR2 DEFAULT 'Yes')
IS
BEGIN
   $TEXTSEARCH
   Client_SYS.Add_To_Attr('DELIVERY_TERMS', del_term_, attr_);
   $TEXTAPPEND
   --(+)230705  TecFarahS  REQ_819-M(START)
   Client_SYS.Add_To_Attr('C_AUTO_CLOSE', CAutoClose_, attr_);
   --(+)230705  TecFarahS  REQ_819-M(FINISH)
   $TEXTEND
END New_Transfer_Order;

-------------------- LU SPECIFIC PROTECTED METHODS --------------------------


-------------------- LU SPECIFIC PUBLIC METHODS -----------------------------

--(+) 231121 TechSurenS MERGE-ENV-ECON-1 (START)
@Override
PROCEDURE Set_Cancel_Reason(order_no_      IN VARCHAR2,
                            cancel_reason_ IN VARCHAR2) IS
   --(+)<100906><ANRAIN><C_Pending Issues for Closure -PNC-2><start>   
   line_cost_    NUMBER := 0;
   exist_budget_ NUMBER := 0;

   CURSOR exist_budget(requisition_no_ VARCHAR2) IS
         SELECT COUNT(*)
           FROM PR_BUDGET_DETAILS_TAB
          WHERE requisition_no = requisition_no_
            AND Purchase_Requisition_Api.Get_Objstate__(REQUISITION_NO) !=
                'Cancelled'; -- 140115 <ADTAIN> C_G1310814-A-1 Budget Functionality to be patched from NHPC 

   CURSOR get_lines IS
         SELECT line_no, release_no, Requisition_No, part_no
           FROM purchase_order_line_tab
          WHERE order_no = order_no_
            AND rowstate != 'Cancelled';

   CURSOR GetReqNo IS
         SELECT DISTINCT T.REQUISITION_NO "REQUISITION_NO"
           FROM PURCHASE_ORDER_LINE_TAB t
          WHERE t.order_no = order_no_
            AND Purchase_Order_Api.Get_Objstate__(t.order_no) != 'Cancelled'
            AND Purchase_Requisition_Api.Get_Objstate__(t.requisition_no) !=
                'Cancelled';
BEGIN
   IF (Purchase_Order_API.Get_Objstate__(order_no_) = 'Planned' AND
       nvl(Purchase_Order_API.Get_Revision_Open(order_no_), 'FALSE') =
       'TRUE') OR Purchase_Order_API.Get_Objstate__(order_no_) = 'Released' THEN
      FOR i IN get_lines LOOP
         exist_budget_ := NULL;
         dbms_output.put_line(' Requisition_No_===' || i.Requisition_No);
         IF i.Requisition_No IS NOT NULL THEN
            OPEN exist_budget(i.Requisition_No);
            FETCH exist_budget
          INTO exist_budget_;
            CLOSE exist_budget;
            IF nvl(exist_budget_, 0) >= 1 THEN
               line_cost_ := 0;
               IF (i.part_no IS NOT NULL) THEN
                  line_cost_ := nvl(Purchase_Order_Line_API.Get_Line_Total(order_no_,
                                                                           i.line_no,
                                                                           i.release_no),
                                    0);
               ELSE
                  line_cost_ := nvl(Purchase_Order_Line_Nopart_API.Get_Line_Total(order_no_,
                                                                                  i.line_no,
                                                                                  i.release_no),
                                    0);
               END IF;

               dbms_output.put_line('line_cost_ = ' || line_cost_);

               IF line_cost_ > 0 THEN
                  line_cost_ := -1 * line_cost_;

                  Pr_Budget_Details_API.BookAmount(i.Requisition_No,
                                                   'PO',
                                                   line_cost_,
                                                   order_no_);
               END IF;

            END IF;
         END IF;
      END LOOP;
   END IF;
   super(order_no_, cancel_reason_);
   FOR i IN GetReqNo LOOP
      Pr_Budget_Details_API.Refresh_Budget(i.REQUISITION_NO);
   END LOOP;
END Set_Cancel_Reason;



@Override
PROCEDURE New (
   order_no_     IN OUT VARCHAR2,
   vendor_no_    IN     VARCHAR2,
   contract_     IN     VARCHAR2,
   buyer_        IN     VARCHAR2,
   authorizer_   IN     VARCHAR2,
   source_order_ IN     VARCHAR2 DEFAULT NULL,
   prefix_       IN VARCHAR2 DEFAULT NULL )
IS
BEGIN
   GPoPrefix_	:= prefix_;      -- India Localisation                                         
   super(order_no_, vendor_no_, contract_, buyer_, authorizer_, source_order_);
   GPoPrefix_	:= NULL;   -- India Localisation
EXCEPTION   -- India Localisation
   WHEN others THEN                          
   GPoPrefix_	:= NULL; 
   RAISE; -- ajshin 081103    
END New;



-------------------- LU EXT NEW METHODS -------------------------------------


-- ADDED BY GIRISH 13-FEB-2003 BEGIN

PROCEDURE Update_PoOrderLines(order_no_ VARCHAR2) IS
   attr_       VARCHAR2(2000);
   info_       VARCHAR2(2000);
   objid_      VARCHAR2(2000);
   objversion_ VARCHAR2(2000);

   CURSOR orderlines IS
    SELECT fbuy_unit_price, objid, objversion
      FROM purchase_order_line_part
     WHERE order_no = order_no_;
BEGIN
   FOR I IN orderlines LOOP
      Client_SYS.Clear_Attr(attr_);
      Client_SYS.Add_To_Attr('CLOSE_CODE', 'Manual', attr_);
      Client_SYS.Add_To_Attr('FBUY_UNIT_PRICE', I.fbuy_unit_price, attr_);
      Purchase_Order_Line_Part_API.Modify__(info_,i.objid,i.objversion,attr_, 'DO');
   END LOOP;

END Update_PoOrderLines;

PROCEDURE CopyWithPrefix__(info_                   IN OUT VARCHAR2,
                           destination_order_      IN OUT VARCHAR2,
                           new_delivery_date_      IN OUT DATE,
                           order_no_               IN     VARCHAR2,
                           new_supplier_           IN     VARCHAR2,
                           new_blanket_            IN     VARCHAR2,
                           include_doc_texts_      IN     VARCHAR2,
                           include_internal_notes_ IN     VARCHAR2,
                           include_pre_accounting_ IN     VARCHAR2,
                           nprefix_                IN     VARCHAR2) IS
BEGIN
   -- Karam
   Purchase_Order_API.GPrefix_ := nprefix_;
   Purchase_Order_API.GPOQty_ := NULL;

   Copy__(info_,
          destination_order_,
          new_delivery_date_,
          order_no_,
          new_supplier_,
          new_blanket_,
          include_doc_texts_,
          include_internal_notes_,
          include_pre_accounting_);

   Purchase_Order_API.GPrefix_ := NULL;
   Purchase_Order_API.Prefix_ := NULL;

END CopyWithPrefix__;

PROCEDURE ChangeToPlanned__(info_       OUT    VARCHAR2,
                            objid_      IN     VARCHAR2,
                            objversion_ IN OUT VARCHAR2,
                            attr_       IN OUT VARCHAR2,
                            action_     IN     VARCHAR2) IS
   rec_ PURCHASE_ORDER_TAB%ROWTYPE;
BEGIN
   IF (action_ = 'CHECK') THEN
      NULL;
   ELSIF (action_ = 'DO') THEN
      rec_ := Lock_By_Id___(objid_, objversion_);
      Finite_State_Machine___(rec_, 'Planned', attr_);
      objversion_ := to_char(rec_.rowversion, 'YYYYMMDDHH24MISS');
      Finite_State_Add_To_Attr___(rec_, attr_);
   END IF;
   info_ := Client_SYS.Get_All_Info;
END ChangeToPlanned__;


PROCEDURE Get_Control_Type_Value_Desc(description_ OUT VARCHAR2,
                                      company_     IN  VARCHAR2,
                                      value_       IN  VARCHAR2) IS
BEGIN
   description_ := value_;
END Get_Control_Type_Value_Desc;


-------------Added By Rohit----------------------------------------

PROCEDURE Revision(order_no_           IN VARCHAR2,
                   chg_order_no_       IN VARCHAR2,
                   server_data_change_ IN NUMBER) IS
   revision_created_ VARCHAR2(5);

   --(+)<100811><ANRAIN><C_Pending Issues for Closure -PNC-1><start>   
   line_cost_       NUMBER := 0;
   budget_category_ DEPARTMENT_BUDGET_TAB.Budget_Category%TYPE;
   fin_year_        DEPARTMENT_BUDGET_TAB.fin_year%TYPE;
   cost_center_     DEPARTMENT_BUDGET_TAB.cost_center%TYPE;
   company_         DEPARTMENT_BUDGET_TAB.company%TYPE;

   CURSOR GetReqNo IS
    SELECT DISTINCT T.REQUISITION_NO "REQUISITION_NO"
      FROM PURCHASE_ORDER_LINE_TAB t
     WHERE t.order_no = order_no_
       AND Purchase_Order_Api.Get_Objstate__(t.order_no) != 'Cancelled' -- 140115 <ADTAIN> C_G1310814-A-1 Budget Functionality to be patched from NHPC
       AND Purchase_Requisition_Api.Get_Objstate__(t.requisition_no) !=
           'Cancelled'; -- 140115 <ADTAIN> C_G1310814-A-1 Budget Functionality to be patched from NHPC

   CURSOR GetPO(requisition_no_ VARCHAR2) IS
    SELECT *
      FROM Purchase_order_Line_tab t
     WHERE t.order_no = order_no_
       AND t.requisition_no = requisition_no_
       AND t.rowstate != 'Cancelled'; -- 140115 <ADTAIN> C_G1310814-A-1 Budget Functionality to be patched from NHPC

   CURSOR exist_budget(requisition_no_ VARCHAR2) IS
    SELECT budget_category, fin_year, cost_center, company
      FROM PR_BUDGET_DETAILS_TAB
     WHERE requisition_no = requisition_no_
       AND Purchase_Requisition_Api.Get_Objstate__(REQUISITION_NO) !=
           'Cancelled'; -- 140115 <ADTAIN> C_G1310814-A-1 Budget Functionality to be patched from NHPC 
   --(+)<100811><ANRAIN><C_Pending Issues for Closure -PNC-1><end>                                

BEGIN
   revision_created_ := 'TRUE';
   --Note: Set all existing lines to Unchanged
   InsertRevInfo(order_no_);
   Purch_Rev_Status_Util_API.Reset_Lines(order_no_,Purch_Revision_Status_API.Decode('UNCHANGED'));
   --Note: Create a new revision
   Purchase_Order_API.Modify_Revision_No(order_no_,chg_order_no_,server_data_change_);

   dbms_output.put_line('order no ' || order_no_);
   dbms_output.put_line('EEEEEEEEEEEEEEEEEEEEEE');
   --(+)<100811><ANRAIN><C_Pending Issues for Closure -PNC-1><start>
   ---reverseing the PR amount booked
   FOR i IN GetReqNo LOOP
      budget_category_ := NULL;
      fin_year_        := NULL;
      cost_center_     := NULL;
      company_         := NULL;
      Dbms_Output.put_line('************' || i.REQUISITION_NO ||'************');
      OPEN exist_budget(i.REQUISITION_NO);
      FETCH exist_budget
      INTO budget_category_, fin_year_, cost_center_, company_;
      CLOSE exist_budget;

      dbms_output.put_line('budget_category_ =' || budget_category_ ||
                           ' fin_year_=' || fin_year_ || ' cost_center_=' ||
                           cost_center_ || ' company_=' || company_);
      IF budget_category_ IS NOT NULL THEN
         FOR j IN GetPO(i.REQUISITION_NO) LOOP
            line_cost_ := 0;
            IF (j.part_no IS NOT NULL) THEN
               line_cost_ := nvl(Purchase_Order_Line_API.Get_Line_Total(j.order_no,j.line_no,j.release_no),0);
            ELSE
               line_cost_ := nvl(Purchase_Order_Line_Nopart_API.Get_Line_Total(j.order_no, j.line_no, j.release_no), 0);
            END IF;
            IF line_cost_ > 0 THEN
               Pr_Budget_Details_API.BookAmount(i.REQUISITION_NO, 'PO', -1 * line_cost_, order_no_);
            END IF;
         END LOOP;
         Pr_Budget_Details_API.Refresh_Budget(i.REQUISITION_NO);
      END IF;
   END LOOP;
   --(+)<100811><ANRAIN><C_Pending Issues for Closure -PNC-1><end>      
END Revision;

---- Added by Rohit----------------------
FUNCTION Change_Status5(order_no_ VARCHAR2) RETURN VARCHAR2 IS
   flag5_        VARCHAR2(5) DEFAULT 'FALSE';
   var_ VARCHAR2(10);

   CURSOR Get_State(order_no_ VARCHAR2, st_ VARCHAR2) IS
    SELECT state
      FROM purchase_order_line_part
     WHERE order_no = order_no_
       AND state NOT IN ('Closed', 'Cancelled')
       AND state = st_
    UNION
    SELECT state
      FROM purchase_order_line_nopart
     WHERE order_no = order_no_
       AND state NOT IN ('Closed', 'Cancelled')
       AND state = st_;
BEGIN
   FOR i IN Get_State(order_no_, 'Received') LOOP
      var_ := 'Received';
      RETURN var_;
      flag5_ := 'TRUE';
      EXIT;
   END LOOP;

   IF (flag5_ = 'FALSE') THEN
      FOR i IN Get_State(order_no_, 'Arrived') LOOP
         var_ := 'Arrived';
         RETURN var_;
         flag5_ := 'TRUE';
         EXIT;
      END LOOP;
   END IF;

   IF (flag5_ = 'FALSE') THEN
      FOR i IN Get_State(order_no_, 'Confirmed') LOOP
         var_ := 'Confirm';
         RETURN var_;
         flag5_ := 'TRUE';
         EXIT;
      END LOOP;
   END IF;

   IF (flag5_ = 'FALSE') THEN
      FOR i IN Get_State(order_no_, 'Released') LOOP
         var_ := 'Released';
         RETURN var_;
         flag5_ := 'TRUE';
         EXIT;
      END LOOP;
   END IF;

   IF (flag5_ = 'FALSE') THEN
      FOR i IN Get_State(order_no_, 'Stopped') LOOP
         var_ := 'Stopped';
         RETURN var_;
         flag5_ := 'TRUE';
         EXIT;
      END LOOP;
   END IF;

   IF (flag5_ = 'FALSE') THEN
      FOR i IN Get_State(order_no_, 'Planned') LOOP
         var_ := 'Planned';
         RETURN var_;
         flag5_ := 'TRUE';
         EXIT;
      END LOOP;
   END IF;
   IF (flag5_ = 'FALSE') THEN
           var_ := 'Released';
            RETURN var_;
            flag5_ := 'TRUE';
           
   END IF;   
END Change_Status5;
-------------end By Rohit--------------------------------


--India Karamdev
PROCEDURE Get_Vendor_No_Rec_Date(order_no_            IN  VARCHAR2,
                                 vendor_no_           OUT VARCHAR2,
                                 wanted_receipt_date_ OUT DATE) IS
   CURSOR get_attr IS
    SELECT vendor_no, wanted_receipt_date
      FROM PURCHASE_ORDER_TAB
     WHERE order_no = order_no_;
BEGIN
   OPEN get_attr;
   FETCH get_attr
    INTO vendor_no_, wanted_receipt_date_;
   CLOSE get_attr;
END Get_Vendor_No_Rec_Date;

FUNCTION Get_Total_Charge(contract_      VARCHAR2,
                          order_no_      VARCHAR2,
                          variable_name_ VARCHAR2) RETURN NUMBER IS
   temp_      NUMBER;
   total_tax_ NUMBER := 0;
   CURSOR get_lines(site_ IN VARCHAR2, ord_ IN VARCHAR2) IS
    SELECT line_no,
           release_no,
           fbuy_unit_price,
           buy_qty_due,
           currency_code,
           conv_factor,
           price_conv_factor,
           currency_rate
      FROM purchase_order_line_tab
     WHERE order_no = ord_
       AND rowstate <> 'Cancelled';

   CURSOR k_get_charges_rate(knext_order_no_ IN VARCHAR2,
                             kline_no_       IN VARCHAR2,
                             kprintdate_     IN VARCHAR2,
                             kreleaseno_     IN NUMBER) IS
    SELECT DISTINCT rate,
                    substr(c.description, 1, 20) strdesc,
                    b.strcode,
                    b.rank,
                    b.tax_rate_type
      FROM PURCHASE_ORDER_LINE_TAB a,
           purch_line_tax_tab      b,
           dist_tax_code_tab       c
     WHERE a.order_no = b.order_no
       AND a.order_no = knext_order_no_
       AND a.line_no = b.line_no
       AND a.line_no = kline_no_
       AND a.release_no = b.release_no
       AND a.release_no = kreleaseno_
       AND c.tax_code = b.tax_code
       AND c.tax_type = b.tax_type
       AND nvl(rate, 0) != 0
       AND SYSDATE BETWEEN trunc(b.from_date) AND nvl(b.till_date, SYSDATE)
     ORDER BY B.RANK;

  dflag_ VARCHAR2(1) := 'Y';

   CURSOR Get_Taxes(order_no_ IN VARCHAR2,
                    line_no_  IN VARCHAR2,
                    rel_no_   IN VARCHAR2) IS
    SELECT SUM(decode(variable_name_,
                      'TAX_AMT',
                      (nvl(tax_amt, 0)),
                      'TAX_CURR_AMT',
                      (nvl(tax_curr_amt, 0)),
                      (nvl(tax_amt, 0))))
      FROM purch_line_tax_tab a
     WHERE a.order_no = order_no_
       AND a.line_no = line_no_
       AND a.release_no = rel_no_
       AND a.payable!='NP';

BEGIN
   FOR i IN get_lines(contract_, order_no_) LOOP
      IF Indian_Charges_D_API.GDisplay_ = 'Y' THEN
      dbms_output.put_line('order_no/Line/Release :: ' || order_no_ || '/' ||
                           i.line_no || '/' || i.release_no);
         dbms_output.put_line('~           Qty/Price :: ' ||
                              to_char(i.buy_qty_due) || '/' ||
                              to_char(i.fbuy_unit_price));
      END IF;

      dbms_output.put_line(' GIndianCharges_ =  ' || GIndianCharges_);
      IF Purchase_Order_API.GIndianCharges_ = 'FALSE' OR
         C_Ind_Tax_Doc_Ctrl_API.Get_New_Logic('PO') = 'TRUE' THEN
         temp_ := 0;

         OPEN Get_Taxes(order_no_, i.line_no, i.release_no);
         FETCH Get_Taxes
        INTO temp_;
         CLOSE Get_Taxes;

         temp_      := ROUND(nvl(temp_, 0), Amount_Round_API.Get_Db_Value(0));
         total_tax_ := nvl(total_tax_, 0) + nvl(temp_, 0);

         dbms_output.put_line('  ' || variable_name_ || ' =  ' || order_no_ || '/' ||
                              i.line_no || '/' || i.release_no || '=' ||
                              temp_);
         dbms_output.put_line('~             Sub Total Tax is  ' ||
                              nvl(total_tax_, 0));
      ELSE
         FOR j IN k_get_charges_rate(order_no_,
                                     i.line_no,
                                     sysdate,
                                     i.release_no) LOOP
            BEGIN
          temp_  := Indian_Charges_D_API.return_rank_price(contract_,
                                                           order_no_,
                                                           i.line_no,
                                                           i.release_no,
                                                           NULL,
                                                           NULL,
                                                           j.strcode,
                                                           i.fbuy_unit_price /
                                                           Nvl(i.price_conv_factor,
                                                               1),
                                                           NULL,
                                                           i.buy_qty_due,
                                                           NULL,
                                                           sysdate,
                                                           i.currency_rate,
                                                           i.currency_code,
                                                           1, -- i.price_conv_factor,
                                                           j.rank,
                                                           'PO',
                                                           dflag_,
                                                           variable_name_);
               dflag_ := 'N';
               temp_  := ROUND(nvl(temp_, 0) * nvl(i.buy_qty_due, 0),
                               Amount_Round_API.Get_Db_Value(0));
            END;
            IF Indian_Charges_D_API.GDisplay_ = 'Y' THEN
               dbms_output.put_line('~             Rank ' || To_Char(J.Rank) ||
                                    ' Tax is  ' || temp_);
            END IF;
            total_tax_ := nvl(total_tax_, 0) + temp_;
            IF Indian_Charges_D_API.GDisplay_ = 'Y' THEN
               dbms_output.put_line('~             Sub Total Tax is  ' ||
                                    nvl(total_tax_, 0));
            END IF;
         END LOOP;
      END IF;
   END LOOP;
   RETURN nvl(total_tax_, 0);
END Get_Total_Charge;

FUNCTION Get_Total_Charge_With_Tolr(contract_      VARCHAR2,
                                    order_no_      VARCHAR2,
                                    variable_name_ VARCHAR2) RETURN NUMBER IS
   temp_      NUMBER;
   total_tax_ NUMBER := 0;

   CURSOR get_lines(site_ IN VARCHAR2, ord_ IN VARCHAR2) IS
    SELECT line_no,
           release_no,
           fbuy_unit_price,
           (buy_qty_due + (Nvl(CLOSE_TOLERANCE, 0) / 100)) buy_qty_due,
           currency_code,
           conv_factor,
           price_conv_factor,
           currency_rate
      FROM purchase_order_line_tab
     WHERE order_no = ord_
       AND rowstate <> 'Cancelled';

   CURSOR k_get_charges_rate(knext_order_no_ IN VARCHAR2,
                             kline_no_       IN VARCHAR2,
                             kprintdate_     IN VARCHAR2,
                             kreleaseno_     IN NUMBER) IS
    SELECT DISTINCT rate,
                    substr(c.description, 1, 20) strdesc,
                    b.strcode,
                    b.rank,
                    b.tax_rate_type
      FROM PURCHASE_ORDER_LINE_TAB a,
           purch_line_tax_tab      b,
           dist_tax_code_tab       c
     WHERE a.order_no = b.order_no
       AND a.order_no = knext_order_no_
       AND a.line_no = b.line_no
       AND a.line_no = kline_no_
       AND a.release_no = b.release_no
       AND a.release_no = kreleaseno_
       AND c.tax_code = b.tax_code
       AND c.tax_type = b.tax_type
       AND nvl(rate, 0) != 0
       AND SYSDATE BETWEEN trunc(b.from_date) AND nvl(b.till_date, SYSDATE)
     ORDER BY B.RANK;

  dflag_ VARCHAR2(1) := 'Y';

   CURSOR Get_Taxes(order_no_ IN VARCHAR2,
                    line_no_  IN VARCHAR2,
                    rel_no_   IN VARCHAR2) IS
    SELECT SUM(decode(variable_name_,
                      'TAX_AMT',
                      (nvl(tax_amt, 0)),
                      'TAX_CURR_AMT',
                      (nvl(tax_curr_amt, 0)),
                      (nvl(tax_amt, 0))))
      FROM purch_line_tax_tab a
     WHERE a.order_no = order_no_
       AND a.line_no = line_no_
       AND a.release_no = rel_no_;
BEGIN
   FOR i IN get_lines(contract_, order_no_) LOOP
      IF Indian_Charges_D_API.GDisplay_ = 'Y' THEN
      dbms_output.put_line('order_no/Line/Release :: ' || order_no_ || '/' ||
                           i.line_no || '/' || i.release_no);
         dbms_output.put_line('~           Qty/Price :: ' ||
                              to_char(i.buy_qty_due) || '/' ||
                              to_char(i.fbuy_unit_price));
      END IF;
      dbms_output.put_line(' GIndianCharges_ =  ' || GIndianCharges_);

      IF Purchase_Order_API.GIndianCharges_ = 'FALSE' OR
         C_Ind_Tax_Doc_Ctrl_API.Get_New_Logic('PO') = 'TRUE' THEN

         temp_ := 0;

         OPEN Get_Taxes(order_no_, i.line_no, i.release_no);
         FETCH Get_Taxes
        INTO temp_;
         CLOSE Get_Taxes;

         temp_      := ROUND(nvl(temp_, 0), Amount_Round_API.Get_Db_Value(0));
         total_tax_ := nvl(total_tax_, 0) + nvl(temp_, 0);

         dbms_output.put_line('  ' || variable_name_ || ' =  ' || order_no_ || '/' ||
                              i.line_no || '/' || i.release_no || '=' ||
                              temp_);
         dbms_output.put_line('~             Sub Total Tax is  ' ||
                              nvl(total_tax_, 0));
      ELSE
         FOR j IN k_get_charges_rate(order_no_,
                                     i.line_no,
                                     sysdate,
                                     i.release_no) LOOP
            BEGIN
          temp_  := Indian_Charges_D_API.return_rank_price(contract_,
                                                           order_no_,
                                                           i.line_no,
                                                           i.release_no,
                                                           NULL,
                                                           NULL,
                                                           j.strcode,
                                                           i.fbuy_unit_price /
                                                           Nvl(i.price_conv_factor,
                                                               1),
                                                           NULL,
                                                           i.buy_qty_due,
                                                           NULL,
                                                           sysdate,
                                                           i.currency_rate,
                                                           i.currency_code,
                                                           1, -- i.price_conv_factor,
                                                           j.rank,
                                                           'PO',
                                                           dflag_,
                                                           variable_name_);
               dflag_ := 'N';
               temp_  := ROUND(nvl(temp_, 0) * nvl(i.buy_qty_due, 0),
                               Amount_Round_API.Get_Db_Value(0));
            END;
            IF Indian_Charges_D_API.GDisplay_ = 'Y' THEN
               dbms_output.put_line('~             Rank ' || To_Char(J.Rank) ||
                                    ' Tax is  ' || temp_);
            END IF;
            total_tax_ := nvl(total_tax_, 0) + temp_;
            IF Indian_Charges_D_API.GDisplay_ = 'Y' THEN
               dbms_output.put_line('~             Sub Total Tax is  ' ||
                                    nvl(total_tax_, 0));
            END IF;
         END LOOP;
      END IF;
   END LOOP;
   RETURN nvl(total_tax_, 0);
END Get_Total_Charge_With_Tolr;

PROCEDURE Enumerate_Prefix(lu_conn_list_ OUT VARCHAR2) IS
   list_ VARCHAR2(2000);
   CURSOR getlunull IS
    SELECT Prefix
      FROM SITE_PREFIX_PO_LOV
     WHERE Contract LIKE Nvl(GContract_, '%');
BEGIN
   FOR met IN getlunull LOOP
      list_ := list_ || met.Prefix || Client_SYS.field_separator_;
   END LOOP;
   lu_conn_list_ := list_;
END Enumerate_Prefix;

PROCEDURE SetMultiRowFlag(value_ IN VARCHAR2) IS
BEGIN
   Trace_SYS.Field('Multiple Row', value_);
   Purchase_Order_API.GMultipleRow_ := value_;
END SetMultiRowFlag;

FUNCTION Get_Total_Order_With_Tolr(order_no_ IN VARCHAR2) RETURN NUMBER IS
   contract_         VARCHAR2(5);
   company_          VARCHAR2(20);
   currency_code_    VARCHAR2(5);
   total_order_curr_ NUMBER;

   CURSOR total_in_order_cur_ IS
    SELECT NVL((buy_qty_due + buy_qty_due * (Nvl(CLOSE_TOLERANCE, 0) / 100)) *
               fbuy_unit_price / price_conv_factor *
               (1 - NVL(discount, 0) / 100) + additional_cost_amount,
               0) LineAmount,
           purchase_site
      FROM PURCHASE_ORDER_LINE_TAB
     WHERE order_no = order_no_
       AND ROWSTATE != 'Cancelled';
BEGIN
   FOR I IN total_in_order_cur_ LOOP
      total_order_curr_ := Nvl(total_order_curr_, 0) + Nvl(I.LineAmount, 0);
      contract_         := I.purchase_site;
   END LOOP;

   company_       := Site_API.Get_Company(contract_);
   currency_code_ := Purchase_Order_API.Get_Currency_Code(order_no_);

   RETURN NVL(ROUND(total_order_curr_,
                    Currency_Code_API.Get_Currency_Rounding(company_,
                                                            currency_code_)),
              0);
END Get_Total_Order_With_Tolr;

PROCEDURE InsertRevInfo(order_ IN VARCHAR2) IS
   CURSOR C1 IS
    SELECT * FROM Purchase_Order_tab WHERE Order_No = order_;

   rec_ C1%ROWTYPE;
BEGIN
   OPEN C1;
   FETCH C1
   INTO rec_;
   CLOSE C1;

  INSERT INTO Purchase_Order_Rev_Hist_Tab
    (ORDER_NO,
     REVISION,
     APPROVAL_RULE,
     ADDR_NO,
     AUTHORIZE_CODE,
     AUTHORIZE_ID,
     BLANKET_ORDER,
     BUYER_CODE,
     CONTRACT,
     CURRENCY_CODE,
     DELIVERY_ADDRESS,
     DELIVERY_TERMS,
     LANGUAGE_CODE,
     NOTE_ID,
     SHIP_VIA_CODE,
     PRE_ACCOUNTING_ID,
     VENDOR_NO,
     CHANGE_DATE,
     CONTACT,
     DATE_ENTERED,
     ADDR_1,
     --DELIVERY_TERMS_DESC ,           
     ADDR_2,
     FORWARDER_ID,
     ADDR_3,
     LABEL_NOTE,
     ADDR_4,
     ADDR_5,
     ORDER_CODE,
     NOTE_TEXT,
     ORDER_DATE,
     PICK_LIST_FLAG,
     --PRINTED_FLAG,
     --SHIP_VIA_DESC    ,              
     WANTED_RECEIPT_DATE,
     VENDOR_CO_NO,
     CUSTOMER_PO_NO,
     RECIPIENT_NAME,
     COUNTRY_CODE,
     PAY_TERM_ID,
     BLANKET_DATE,
     SCHEDULE_AGREEMENT_ORDER,
     DESTINATION_ID,
     INTERNAL_DESTINATION,
     ADDRESS1,
     ADDRESS2,
     ZIP_CODE,
     CITY,
     ADDR_STATE,
     ROWSTATE,
     COUNTY,
     ADDR_FLAG,
     REFERENCE,
     CENTRAL_ORDER_FLAG,
     CONSOLIDATED_FLAG,
     FREIGHT_FLAG,
     OPEN_PO,
     GIT_FLAG,
     REVISION_DATE,
     CO_PREFIX,
     LC_NUMBER,
     PAYMENT_METHOD,
     NEGOTIATING_BANK,
     PICKUP_LOCATION,
     PORT_OF_LOADING,
     PORT_OF_DISCHARGE,
     FINAL_DESTINATION,
     ROWVERSION)
  VALUES
    (rec_.ORDER_NO,
     rec_.REVISION,
     rec_.APPROVAL_RULE,
     rec_.ADDR_NO,
     rec_.AUTHORIZE_CODE,
     rec_.AUTHORIZE_ID,
     NULL, --Rec_.BLANKET_ORDER,      -- Blanket_Order Attribute Removed From Purchase_Order_Tab
     rec_.BUYER_CODE,
     rec_.CONTRACT,
     rec_.CURRENCY_CODE,
     rec_.DELIVERY_ADDRESS,
     rec_.DELIVERY_TERMS,
     rec_.LANGUAGE_CODE,
     rec_.NOTE_ID,
     rec_.SHIP_VIA_CODE,
     rec_.PRE_ACCOUNTING_ID,
     rec_.VENDOR_NO,
     rec_.CHANGE_DATE,
     rec_.CONTACT,
     rec_.DATE_ENTERED,
     rec_.ADDRESS1,
     --Rec_.DELIVERY_TERMS_DESC ,           
     rec_.ADDRESS2,
     rec_.FORWARDER_ID,
     rec_.ZIP_CODE,
     rec_.LABEL_NOTE,
     rec_.ADDR_STATE,
     rec_.COUNTY,
     rec_.ORDER_CODE,
     rec_.NOTE_TEXT,
     rec_.ORDER_DATE,
     rec_.PICK_LIST_FLAG,
     --rec_.PRINTED_FLAG,
     --Rec_.SHIP_VIA_DESC    ,              
     rec_.WANTED_RECEIPT_DATE,
     rec_.VENDOR_CO_NO,
     rec_.CUSTOMER_PO_NO,
     rec_.RECIPIENT_NAME,
     rec_.COUNTRY_CODE,
     rec_.PAY_TERM_ID,
     rec_.BLANKET_DATE,
     rec_.SCHEDULE_AGREEMENT_ORDER,
     rec_.DESTINATION_ID,
     rec_.INTERNAL_DESTINATION,
     rec_.ADDRESS1,
     rec_.ADDRESS2,
     rec_.ZIP_CODE,
     rec_.CITY,
     rec_.ADDR_STATE,
     rec_.ROWSTATE,
     rec_.COUNTY,
     rec_.ADDR_FLAG,
     rec_.REFERENCE,
     rec_.CENTRAL_ORDER_FLAG,
     rec_.CONSOLIDATED_FLAG,
     rec_.FREIGHT_FLAG,
     rec_.OPEN_PO,
     rec_.GIT_FLAG,
     SYSDATE, -- Rec_.REVISION_DATE
     rec_.CO_PREFIX,
     rec_.LC_NUMBER,
     rec_.PAYMENT_METHOD,
     rec_.NEGOTIATING_BANK,
     rec_.PICKUP_LOCATION,
     rec_.PORT_OF_LOADING,
     rec_.PORT_OF_DISCHARGE,
     rec_.FINAL_DESTINATION,
     rec_.ROWVERSION);

END InsertRevInfo;

--HAL10003,Start

PROCEDURE Split__(info_ IN OUT VARCHAR2,
                  destination_order_      IN OUT VARCHAR2,
                  new_delivery_date_      IN OUT DATE,
                  order_no_               IN     VARCHAR2,
                  line_no_                IN     VARCHAR2,
                  release_no_             IN     VARCHAR2,
                  new_supplier_           IN     VARCHAR2,
                  new_blanket_            IN     VARCHAR2,
                  include_doc_texts_      IN     VARCHAR2,
                  include_internal_notes_ IN     VARCHAR2,
                  include_pre_accounting_ IN     VARCHAR2,
                  check_existing_         IN     VARCHAR2) IS
   CURSOR orderlines IS
    SELECT line_no,
           release_no,
           a.demand_operation_no,
           a.demand_order_code,
           a.demand_order_no,
           a.demand_order_type,
           a.demand_release,
           a.demand_sequence_no
      FROM purchase_order_line_tab a
     WHERE order_no = order_no_
       AND line_no = line_no_
       AND release_no = release_no_;

   po_contract_ PURCHASE_ORDER_TAB.contract%TYPE;

   CURSOR Get_Struc(order_no_   IN VARCHAR2,
                    line_no_    IN VARCHAR2,
                    release_no_ IN VARCHAR2) IS
    SELECT DISTINCT STRCODE
      FROM Purch_line_Struc_Tab
     WHERE Order_No = order_no_
       AND Line_no = line_no_
       AND Release_No = release_no_;

   CURSOR get_objid IS
    SELECT a.objid, a.objversion
      FROM purchase_order_line a
     WHERE order_no = order_no_
       AND line_no = line_no_
       AND release_no = release_no_;

  strcode_    VARCHAR2(50);
   objid_      VARCHAR2(2000);
   objversion_ VARCHAR2(2000);
   info1_      VARCHAR2(2000);
   attr_       VARCHAR2(2000);
      --ENVNAGANB 010923 <DML Code Correction> (START)
       CURSOR get_objid_(v_order_no_ VARCHAR2,v_line_no VARCHAR2,v_release_no VARCHAR2)IS 
       SELECT objid,Objversion
       FROM PURCHASE_ORDER_LINE
       WHERE ORDER_NO = v_order_no_
       AND line_no = v_line_no
       AND release_no = v_release_no;
      --ENVNAGANB 010923 <DML Code Correction> (FINISH)
BEGIN
   IF Purchase_Order_Line_API.GET_STATE(order_no_, line_no_, release_no_) IN
      ('Cancelled', 'Closed') THEN
      Error_SYS.Record_General(Lu_Name_, 'ORDERNOLINESTATUS: Split is allowed only when line status not equal to Cancelled or Closed');
   END IF;
   dbms_output.put_line('check_existing_: ' || check_existing_);
   po_contract_ := Purchase_Order_API.Get_Contract(order_no_);
   IF check_existing_ IS NOT NULL AND destination_order_ IS NULL THEN
      Error_SYS.Record_General(Lu_Name_,'ORDERNO: Enter the Order No to which the line is to be added');
   END IF;
   IF new_delivery_date_ IS NULL AND check_existing_ IS NOT NULL AND
      destination_order_ IS NOT NULL THEN
      new_delivery_date_ := Purchase_Order_API.Get_Wanted_Receipt_Date(destination_order_);
   ELSE
      new_delivery_date_ := Purchase_Order_API.Get_Wanted_Receipt_Date(order_no_);
   END IF;
   dbms_output.put_line('new_delivery_date_: ' || new_delivery_date_);
   IF check_existing_ IS NULL THEN
      dbms_output.put_line('check_existing1_: ' || check_existing_);
      IF (new_blanket_ IS NOT NULL) THEN
         Supplier_Blanket_API.Exist(new_blanket_);
         IF (Purchase_Blanket_Order_API.Encode(Supplier_Blanket_API.Get_Purchase_Blanket_Order(new_blanket_)) =
             'ORDER') THEN
            Error_SYS.Record_General(lu_name_,
                                     'BLKPONOTALLOWED: Blankets of type Blanket Purchase Order cannot be used this way. Use Create Blanket Purchase Order instead.');
         END IF;
         po_contract_ := Purchase_Order_API.Get_Contract(order_no_);
         IF (Supp_Blk_Site_API.Check_Exist(new_blanket_, po_contract_) = 0) THEN
            Error_SYS.Record_General(lu_name_,
                                     'NOTAVALIDSITE: Site :P1 is not a valid site for Blanket Order :P2 ',
                                     po_contract_,
                                     new_blanket_);
         END IF;
      END IF;
          
      -- Bug 39416, Added info_ parameter
      Copy_Header___(destination_order_,
                     new_delivery_date_,
                     info_,
                     order_no_,
                     new_supplier_,
                     include_doc_texts_,
                     include_internal_notes_,
                     include_pre_accounting_);
                     --new_blanket_);
   END IF;
   dbms_output.put_line('new_delivery_date1_: ' || new_delivery_date_);
   FOR line_rec IN orderlines LOOP
      dbms_output.put_line('eeeeee: ');

      Purchase_Order_API.GStructure_ := NULL;
      strcode_                       := NULL;

      IF Purchase_Order_API.Get_Vendor_No(destination_order_) =
         Purchase_Order_API.Get_Vendor_No(order_no_) THEN
         OPEN Get_Struc(order_no_, Line_Rec.Line_no, line_rec.release_no);
         FETCH Get_Struc
        INTO strcode_;
         CLOSE Get_Struc;
         Trace_SYS.Field('<<<< Structure >>>> ', strcode_);
         IF strcode_ IS NOT NULL THEN
            Purchase_Order_API.GStructure_ := strcode_;
         END IF;
      END IF;
      Purchase_Order_API.Split_Line(line_rec.line_no,
                                    line_rec.release_no,
                                    order_no_,
                                    destination_order_,
                                    new_delivery_date_,
                                    new_supplier_,
                                    include_doc_texts_,
                                    include_internal_notes_,
                                    include_pre_accounting_,
                                    new_blanket_);

      OPEN get_objid;
      FETCH get_objid
      INTO objid_, objversion_;
      CLOSE get_objid;

      --ENVNAGANB 010923 <DML Code Correction> (START)
       OPEN get_objid_(order_no_,line_rec.line_no,line_rec.release_no);
       FETCH get_objid_ INTO Objid_,Objversion_;
       CLOSE get_objid_;
       
      Client_SYS.Clear_Attr(attr_);
      Client_SYS.Add_To_Attr('DEMAND_OPERATION_NO_OLD', line_rec.demand_operation_no, attr_);
      Client_SYS.Add_To_Attr('DEMAND_ORDER_CODE_OLD', line_rec.demand_order_code, attr_);
      Client_SYS.Add_To_Attr('DEMAND_ORDER_NO_OLD', line_rec.demand_order_no, attr_);
      Client_SYS.Add_To_Attr('DEMAND_ORDER_TYPE_OLD', line_rec.demand_order_type, attr_);
      Client_SYS.Add_To_Attr('DEMAND_RELEASE_OLD', line_rec.demand_release, attr_);
      Client_SYS.Add_To_Attr('DEMAND_SEQUENCE_NO_OLD', line_rec.demand_sequence_no, attr_);
      Client_SYS.Add_To_Attr('DEMAND_OPERATION_NO', '', attr_);
      Client_SYS.Add_To_Attr('DEMAND_ORDER_CODE', '', attr_);
      Client_SYS.Add_To_Attr('DEMAND_ORDER_NO', '', attr_);
      Client_SYS.Add_To_Attr('DEMAND_ORDER_TYPE', '', attr_);
      Client_SYS.Add_To_Attr('DEMAND_RELEASE', '', attr_);
      Client_SYS.Add_To_Attr('DEMAND_SEQUENCE_NO', '', attr_);
      Client_SYS.Add_To_Attr('DEMAND_CODE', 'NO', attr_);

      IF Objid_ IS NOT NULL AND Objversion_ IS NOT NULL THEN
         PURCHASE_ORDER_LINE_API.Modify__(info_,Objid_,Objversion_,attr_,'DO');
      END IF;
--ENVNAGANB 010923 <DML Code Correction> (FINISH)

      --ENVNAGANB 010923 <DML Code Correction> (START)
       OPEN get_objid_(destination_order_,line_rec.line_no,line_rec.release_no);
       FETCH get_objid_ INTO Objid_,Objversion_;
       CLOSE get_objid_;
       
      Client_SYS.Clear_Attr(attr_);
      Client_SYS.Add_To_Attr('DEMAND_OPERATION_NO', line_rec.demand_operation_no, attr_);
      Client_SYS.Add_To_Attr('DEMAND_ORDER_CODE', line_rec.demand_order_code, attr_);
      Client_SYS.Add_To_Attr('DEMAND_ORDER_NO', line_rec.demand_order_no, attr_);
      Client_SYS.Add_To_Attr('DEMAND_ORDER_TYPE', line_rec.demand_order_type, attr_);
      Client_SYS.Add_To_Attr('DEMAND_RELEASE', line_rec.demand_release, attr_);
      Client_SYS.Add_To_Attr('DEMAND_SEQUENCE_NO', line_rec.demand_sequence_no, attr_);
      Client_SYS.Add_To_Attr('DEMAND_CODE', 'SO', attr_);
      
      IF Objid_ IS NOT NULL AND Objversion_ IS NOT NULL THEN
         PURCHASE_ORDER_LINE_API.Modify__(info_,Objid_,Objversion_,attr_,'DO');
      END IF;
--ENVNAGANB 010923 <DML Code Correction> (FINISH)
      
      IF Purchase_Order_Line_API.GET_STATE(order_no_,
                                           line_rec.line_no,
                                           line_rec.release_no) != 'Received' THEN
--      ENVNAGANB 010923 <DML Code Correction> (START)                                           
      PURCHASE_ORDER_LINE_API.Set_Rowstate(order_no_,line_rec.line_no,line_rec.release_no,'Cancelled');
--      ENVNAGANB 010923 <DML Code Correction> (FINISH)
      ELSE
         Purchase_Order_Line_API.Close__(info1_,objid_, objversion_, attr_, 'DO');
      END IF;
   END LOOP;

   -- Bug 39416, Include the info_ out from Copy_Header___
   info_ := info_ || Client_SYS.Get_All_Info;
   Purchase_Order_Charge_API.Copy_Charge(order_no_, destination_order_);
END Split__;

PROCEDURE Split_Line(line_no_                IN OUT VARCHAR2,
                     release_no_             IN OUT VARCHAR2,
                     order_no_               IN     VARCHAR2,
                     destination_order_      IN     VARCHAR2,
                     new_delivery_date_      IN     DATE,
                     new_supplier_           IN     VARCHAR2,
                     include_doc_texts_      IN     VARCHAR2,
                     include_internal_notes_ IN     VARCHAR2,
                     include_pre_accounting_ IN     VARCHAR2,
                     new_blanket_            IN     VARCHAR2) IS
   org_note_id_             NUMBER;
   org_pre_accounting_id_   NUMBER;
   new_pre_accounting_id_   NUMBER;
   new_supplier2_           VARCHAR2(10);
   new_blanket2_            VARCHAR2(12);
   blanket_line_            NUMBER;
   fbuy_unit_price_         NUMBER;
   dprice_unit_meas_        VARCHAR2(10);
   ddiscount_               NUMBER;
   dadditional_cost_amount_ NUMBER;
   dprice_conv_factor_      NUMBER;
   dlist_curr_code_         VARCHAR2(20);
   attr_                    VARCHAR2(32000);
   hist_msg_                VARCHAR2(200);
   newrec_                  PURCHASE_ORDER_LINE_TAB%ROWTYPE;
   temp_date_               DATE;
   part_ownership_          VARCHAR2(50);
   temp_pre_accounting_id_  NUMBER;
   head_pre_accounting_id_  NUMBER;
   blanket_date_            VARCHAR2(20);
   price_date_              DATE;
   part_sup_status_code_    VARCHAR2(2);
   purchase_order_rec_      Purchase_Order_API.Public_Rec;
   header_company_          VARCHAR2(20);
   line_company_            VARCHAR2(20);
   single_occ_attr_         VARCHAR2(32000);

   CURSOR SOURCE IS
    SELECT *
      FROM PURCHASE_ORDER_LINE_TAB
     WHERE ROWTYPE = 'PurchaseOrderLinePart'
       AND order_no = order_no_
       AND line_no = line_no_
       AND release_no = release_no_;

   binfo_                   VARCHAR2(2000);
   
   bprice_conv_factor_      purchase_order_line_tab.price_conv_factor%TYPE;
   bprice_unit_meas_        purchase_order_line_tab.price_unit_meas%TYPE;
   bdiscount_               purchase_order_line_tab.discount%TYPE;
   badditional_cost_amount_ purchase_order_line_tab.additional_cost_amount%TYPE;
   bcurrency_code_          purchase_order_line_tab.currency_code%TYPE;
   bcurr_rate_              purchase_order_line_tab.currency_rate%TYPE;
   bbuy_unit_price_         purchase_order_line_tab.buy_unit_price%TYPE;
   bfbuy_unit_price_        purchase_order_line_tab.fbuy_unit_price%TYPE;

BEGIN
  dbms_output.put_line('new_delivery_date_:' || new_delivery_date_);
   OPEN SOURCE;
   FETCH SOURCE
    INTO newrec_;
   IF SOURCE%FOUND THEN
      IF (new_blanket_ IS NOT NULL) THEN
         IF (Purchase_Blanket_Order_API.Encode(Supplier_Blanket_API.Get_Purchase_Blanket_Order(new_blanket_)) =
             'ORDER') THEN
            Error_SYS.Record_General(lu_name_,
                                     'BLKPONOTALLOWED: Blankets of type Blanket Purchase Order cannot be used this way. Use Create Blanket Purchase Order instead.');
         END IF;
      END IF;
      purchase_order_rec_ := Purchase_Order_API.Get(order_no_);
      IF (new_supplier_ IS NOT NULL) THEN
         new_supplier2_ := new_supplier_;
      ELSE
         new_supplier2_ := purchase_order_rec_.vendor_no;
      END IF;
      part_sup_status_code_ := Purchase_Part_Supplier_API.Get_Status_Code(newrec_.contract,
                                                                          newrec_.part_no,
                                                                          new_supplier2_);
      IF ((new_supplier_ IS NULL) AND (part_sup_status_code_ = '2')) OR
         ((new_supplier_ IS NOT NULL) AND
          (Purchase_Part_Supplier_API.Check_Vendor_No(newrec_.contract,
                                                     newrec_.part_no,
                                                     new_supplier2_,
                                                     Purchase_Part_Supplier_API.Get_Primary_Vendor(newrec_.contract,
                                                                                                   newrec_.part_no,
                                                                                                   new_supplier2_)) IS NOT NULL) AND
          (part_sup_status_code_ = '2')) THEN

         Client_SYS.Clear_Attr(attr_);
         Client_SYS.Add_To_Attr('ORDER_NO', destination_order_, attr_);

         DBMS_OUTPUT.PUT_LINE('RRRRRRRRRRRRRRRRRnew_blanket_:' ||
                              new_blanket_);

         IF (new_blanket_ IS NOT NULL) THEN
            new_blanket2_ := new_blanket_;
            blanket_date_ := purchase_order_rec_.blanket_date;
            IF (blanket_date_ = 'ORDERDATE') THEN
               price_date_ := newrec_.date_entered;
            ELSIF (blanket_date_ = 'DUEDATE') THEN
               price_date_ := new_delivery_date_;
            END IF;
            IF (Supp_Blk_Period_API.Is_Within_Valid_Period(new_blanket_,
                                                           price_date_) =
                'FALSE') THEN
               Error_SYS.Record_General(lu_name_,
                                        'BLKNOTVALIDINTIME: Blanket No :P1 is not valid in time.',
                                        new_blanket_);
            END IF;
            IF (Supp_Blk_Period_API.Is_Valid_Blanket(new_blanket_, price_date_) =
                'FALSE') THEN
               Error_SYS.Record_General(lu_name_,
                                        'MAXAMOUNTEXCEED: The maximum amount for Blanket No :P1 has been exceeded for the current period.',
                                        new_blanket_);
            END IF;

            Supp_Blk_Part_API.Get_Blanket_Price(fbuy_unit_price_,
                                                fbuy_unit_price_,
                                                blanket_line_,
                                                ddiscount_,
                                                dadditional_cost_amount_,
                                                dadditional_cost_amount_,
                                                dprice_conv_factor_,
                                                dlist_curr_code_,
                                                dprice_unit_meas_,
                                                new_blanket2_,
                                                newrec_.closest_agrmt_part_assort_line,
                                                newrec_.agrmt_part_assort_line_no,
                                                price_date_,
                                                newrec_.currency_code,
                                                Company_Finance_API.Get_Currency_Code(Site_API.Get_Company(newrec_.purchase_site)),
                                                new_supplier2_,
                                                newrec_.purchase_site,
                                                newrec_.part_no,
                                                NULL,
                                                newrec_.buy_qty_due,
                                                newrec_.order_no,
                                                newrec_.line_no,
                                                newrec_.release_no,
                                                'FALSE');

            IF blanket_line_ IS NOT NULL THEN
               Client_SYS.Add_To_Attr('BLANKET_ORDER', new_blanket_, attr_);
               Client_SYS.Add_To_Attr('BLANKET_LINE', blanket_line_, attr_);
            ELSE
               fbuy_unit_price_ := Purchase_Order_Line_API.Get_Fbuy_Unit_Price(order_no_,
                                                                               line_no_,
                                                                               release_no_);
            END IF;
         ELSE
            DBMS_OUTPUT.PUT_LINE('RRRRRRRRRRRRRRRRRnewrec_.blanket_order:' ||
                                 newrec_.blanket_order);
            DBMS_OUTPUT.PUT_LINE('RRRRRRRRRRRRRRRRRnewrec_.blanket_line:' ||
                                 newrec_.blanket_line);
            --SUPP_BLK_PART_API.Get_Blanket(newrec_.blanket_order,new_supplier_,sysdate,newrec_.currency_code,newrec_.contract,newrec_.part_no);

            newrec_.blanket_line  := NULL;
            newrec_.blanket_order := NULL;

            Purchase_Order_Line_Part_API.get_price_info(price_conv_factor_      => bprice_conv_factor_,
                                                        price_unit_meas_        => bprice_unit_meas_,
                                                        discount_               => bdiscount_,
                                                        additional_cost_amount_ => badditional_cost_amount_,
                                                        additional_cost_incl_tax_ => badditional_cost_amount_,
                                                        curr_code_              => bcurrency_code_,
                                                        curr_rate_              => bcurr_rate_,
                                                        buy_unit_price_         => bbuy_unit_price_,
                                                        buy_unit_price_incl_tax_=> bbuy_unit_price_ ,
                                                        fbuy_unit_price_        => bfbuy_unit_price_,
                                                        fbuy_unit_price_incl_tax_=> bfbuy_unit_price_,
                                                        blanket_line_           => newrec_.blanket_line,
                                                        blanket_order_          => newrec_.blanket_order,
                                                        info_                   => binfo_,
                                                        closest_part_assort_line_ => newrec_.closest_agrmt_part_assort_line ,
                                                        agrmt_part_assort_line_=> newrec_.agrmt_part_assort_line_no,
   
                                                        part_no_                => newrec_.part_no,
                                                        contract_               => newrec_.contract,
                                                        vendor_no_              => new_supplier_,
                                                        qty_purchase_           => Purch_Consign_Head_API.get_due_consign_qty(order_no_,
                                                                                                                              line_no_,
                                                                                                                              release_no_),
                                                        price_date_             => TRUNC(sysdate),
                                                        currency_type_ => newrec_.currency_type,
                                                        order_no_ => newrec_.order_no,
                                                        line_no_ => newrec_.line_no,
                                                        release_no_ => newrec_.release_no,
                                                        project_id_ => newrec_.project_id,
                                                        service_type_=> newrec_.service_type,
                                                        condition_code_=> newrec_.condition_code,
                                                        exchange_item_=> newrec_.exchange_item,
                                                        consolidated_=> 'FALSE' ,
                                                        part_ownership_=> newrec_.part_ownership ,
                                                        invoicing_supplier_=> newrec_.invoicing_supplier,
                                                        rental_chargable_days_=> NULL,
                                                        show_zero_price_msg_ =>'TRUE');

            DBMS_OUTPUT.PUT_LINE('RRRRRRRRRRRRRRRRRnewrec_.blanket_order1:' ||
                                 newrec_.blanket_order);
            DBMS_OUTPUT.PUT_LINE('RRRRRRRRRRRRRRRRRnewrec_.blanket_line1:' ||
                                 newrec_.blanket_line);

            Client_SYS.Add_To_Attr('BLANKET_ORDER',
                                   newrec_.blanket_order,
                                   attr_);
            Client_SYS.Add_To_Attr('BLANKET_LINE', newrec_.blanket_line, attr_);
            fbuy_unit_price_ := nvl(bfbuy_unit_price_,
                                    Purchase_Order_Line_API.Get_Fbuy_Unit_Price(order_no_,
                                                                                line_no_,
                                                                                release_no_));
         END IF;

         Client_SYS.Add_To_Attr('FBUY_UNIT_PRICE',
                                nvl(bfbuy_unit_price_, fbuy_unit_price_),
                                attr_);

         Client_SYS.Add_To_Attr('CLOSE_CODE',
                                Purch_Close_Code_API.Decode(newrec_.close_code),
                                attr_);
         Client_SYS.Add_To_Attr('DATE_ENTERED',
                                Site_API.Get_Site_Date(newrec_.contract),
                                attr_);
         Client_SYS.Add_To_Attr('LAST_ACTIVITY_DATE',
                                Site_API.Get_Site_Date(newrec_.contract),
                                attr_);

         IF (include_internal_notes_ = 'Y') THEN
            Client_SYS.Add_To_Attr('NOTE_TEXT', newrec_.note_text, attr_);
         END IF;
         temp_date_      := NULL;
         part_ownership_ := Purchase_Order_Line_Part_API.Get_Part_Ownership(order_no_,
                                                                            line_no_,
                                                                            release_no_);

         IF newrec_.order_code != '6' THEN
            newrec_.order_code := '1';
         END IF;

         dbms_output.put_line('new_delivery_date_:' || new_delivery_date_);
         Client_SYS.Add_To_Attr('PART_OWNERSHIP', part_ownership_, attr_);
         Client_SYS.Add_To_Attr('OWNING_CUSTOMER_NO',
                                newrec_.owning_customer_no,
                                attr_);
         Client_SYS.Add_To_Attr('QTY_ON_ORDER', 0, attr_);
         Client_SYS.Add_To_Attr('REVISED_QTY', 0, attr_);
         Client_SYS.Add_To_Attr('SAMPLE_PERCENT',
                                newrec_.sample_percent,
                                attr_);
         Client_SYS.Add_To_Attr('SAMPLE_QTY', newrec_.sample_qty, attr_);
         Client_SYS.Add_To_Attr('WANTED_DELIVERY_DATE', temp_date_, attr_);
         Client_SYS.Add_To_Attr('CLOSE_TOLERANCE',
                                newrec_.close_tolerance,
                                attr_);
         Client_SYS.Add_To_Attr('PLANNED_DELIVERY_DATE', temp_date_, attr_);
         Client_SYS.Add_To_Attr('PLANNED_RECEIPT_DATE',
                                new_delivery_date_,
                                attr_);
         Client_SYS.Add_To_Attr('PROMISED_DELIVERY_DATE', temp_date_, attr_);
         Client_SYS.Add_To_Attr('LINE_NO', newrec_.line_no, attr_);
         Client_SYS.Add_To_Attr('RELEASE_NO', newrec_.release_no, attr_);
         Client_SYS.Add_To_Attr('ENG_CHG_LEVEL', newrec_.eng_chg_level, attr_);
         Client_SYS.Add_To_Attr('BUYER_CODE', newrec_.buyer_code, attr_);
         Client_SYS.Add_To_Attr('BUY_UNIT_MEAS', newrec_.buy_unit_meas, attr_);

         -- Client_SYS.Add_To_Attr('BUY_QTY_DUE', newrec_.buy_qty_due, attr_);
         Client_SYS.Add_To_Attr('BUY_QTY_DUE',
                                Purch_Consign_Head_API.get_due_consign_qty(order_no_,
                                                                           line_no_,
                                                                           release_no_),
                                attr_);
         Client_SYS.Add_To_Attr('CONTRACT', newrec_.contract, attr_);
         Client_SYS.Add_To_Attr('CURRENCY_CODE',
                                nvl(bcurrency_code_, newrec_.currency_code),
                                attr_);
         Client_SYS.Add_To_Attr('INSPECTION_CODE',
                                newrec_.inspection_code,
                                attr_);
         Client_SYS.Add_To_Attr('ORDER_CODE', newrec_.order_code, attr_);
         Client_SYS.Add_To_Attr('PART_NO', newrec_.part_no, attr_);
         Client_SYS.Add_To_Attr('PRICE_UNIT_MEAS',
                                nvl(bprice_unit_meas_, newrec_.price_unit_meas),
                                attr_);
         Client_SYS.Add_To_Attr('QC_CODE', newrec_.qc_code, attr_);
         Client_SYS.Add_To_Attr('UNIT_MEAS', newrec_.unit_meas, attr_);
         Client_SYS.Add_To_Attr('ADDITIONAL_COST_AMOUNT',
                                nvl(badditional_cost_amount_,
                                    newrec_.additional_cost_amount),
                                attr_);
         Client_SYS.Add_To_Attr('BUY_UNIT_PRICE',
                                nvl(bbuy_unit_price_, newrec_.buy_unit_price),
                                attr_);
         Client_SYS.Add_To_Attr('CONV_FACTOR', newrec_.conv_factor, attr_);
         -- Client_SYS.Add_To_Attr('CURRENCY_RATE',Purchase_Req_Util_API.Get_Currency_Rate(newrec_.currency_code, newrec_.purchase_site), attr_);
         Client_SYS.Add_To_Attr('CURRENCY_RATE',
                                NVL(bcurr_rate_,
                                    Purchase_Req_Util_API.Get_Currency_Rate(newrec_.currency_code,
                                                                            newrec_.purchase_site)),
                                attr_);

         Client_SYS.Add_To_Attr('DESCRIPTION', newrec_.description, attr_);
         Client_SYS.Add_To_Attr('DISCOUNT',
                                nvl(bdiscount_, newrec_.discount),
                                attr_);
         Client_SYS.Add_To_Attr('ORIGINAL_QTY', newrec_.original_qty, attr_);
         Client_SYS.Add_To_Attr('PRICE_CONV_FACTOR',
                                nvl(bprice_conv_factor_,
                                    newrec_.price_conv_factor),
                                attr_);
         Client_SYS.Add_To_Attr('DELIVERY_CONTROL_CODE',
                                newrec_.delivery_control_code,
                                attr_);
         Client_SYS.Add_To_Attr('AUTOMATIC_INVOICE_DB',
                                newrec_.automatic_invoice,
                                attr_);
         Client_SYS.Add_To_Attr('TECHNICAL_COORDINATOR_ID',
                                newrec_.technical_coordinator_id,
                                attr_);
         Client_SYS.Add_To_Attr('CONFIGURATION_ID',
                                newrec_.configuration_id,
                                attr_);
         Client_SYS.Add_To_Attr('PROCESS_TYPE', newrec_.process_type, attr_);
         Client_SYS.Add_To_Attr('TAXABLE', newrec_.taxable, attr_);
         Client_SYS.Add_To_Attr('MANUFACTURER_PART_NO',
                                newrec_.manufacturer_part_no,
                                attr_);
         Client_SYS.Add_To_Attr('MANUFACTURER_ID',
                                newrec_.manufacturer_id,
                                attr_);
         Client_SYS.Add_To_Attr('ADDR_FLAG_DB', newrec_.addr_flag, attr_);
         Client_SYS.Add_To_Attr('DEFAULT_ADDR_FLAG_DB',
                                newrec_.default_addr_flag,
                                attr_);
         Client_SYS.Add_To_Attr('ADDRESS_ID', newrec_.address_id, attr_);
         Client_SYS.Add_To_Attr('ADDR_FLAG_DB', newrec_.addr_flag, attr_);
         org_note_id_    := newrec_.note_id;
         newrec_.note_id := Document_Text_API.Get_Next_Note_Id;
         Client_SYS.Add_To_Attr('NOTE_ID', newrec_.note_id, attr_);
         Client_SYS.Add_To_Attr('IS_EXCHANGE_PART',
                                newrec_.is_exchange_part,
                                attr_);
         Client_SYS.Add_To_Attr('CONDITION_CODE',
                                newrec_.condition_code,
                                attr_);
         Client_SYS.Add_To_Attr('EXCHANGE_ITEM_DB',
                                newrec_.exchange_item,
                                attr_);
         Client_SYS.Add_To_Attr('CORE_DEPOSIT', newrec_.core_deposit, attr_);
         Client_SYS.Add_To_Attr('CORE_DEPOSIT_BASE',
                                newrec_.core_deposit_base,
                                attr_);

         IF (include_pre_accounting_ = 'Y') THEN
            --Note: Making sure that pre_accounting_id is NOT NULL will avoid the automatic copying
            --Note: of the pre accounting from the header.
            new_pre_accounting_id_ := Pre_Accounting_API.Get_Next_Pre_Accounting_Id;
            Client_SYS.Add_To_Attr('PRE_ACCOUNTING_ID',
                                   new_pre_accounting_id_,
                                   attr_);
         ELSE
            --Note: Making sure that pre_accounting_id is NULL will make the automatic copying
            --Note: of the pre accounting from the header take place.
            Client_SYS.Set_Item_Value('PRE_ACCOUNTING_ID',
                                      To_Number(NULL),
                                      attr_);
         END IF;

         IF (newrec_.order_code = 6) THEN
            Client_SYS.Add_To_Attr('SERVICE_TYPE', newrec_.service_type, attr_);
            Client_SYS.Add_To_Attr('SERIAL_NO', newrec_.serial_no, attr_);
            Client_SYS.Add_To_Attr('LOT_BATCH_NO', newrec_.lot_batch_no, attr_);
         END IF;

         -- Bug 47331, start
         Client_SYS.Add_To_Attr('COPY_CHARGE', 'TRUE', attr_);

         Purchase_Order_Line_Part_API.New(attr_);

         -- Note: Copy Single Occurence address details
         IF (newrec_.default_addr_flag = 'N') AND (newrec_.addr_flag = 'Y') THEN
            Client_SYS.Clear_Attr(single_occ_attr_);
            Client_SYS.Add_To_Attr('ORDER_NO',
                                   destination_order_,
                                   single_occ_attr_);
            Client_SYS.Add_To_Attr('LINE_NO',
                                   newrec_.line_no,
                                   single_occ_attr_);
            Client_SYS.Add_To_Attr('RELEASE_NO',
                                   newrec_.release_no,
                                   single_occ_attr_);
            Client_SYS.Add_To_Attr('ADDR_NAME',
                                   Pur_Order_Line_Address_API.Get_Addr_Name(newrec_.order_no,
                                                                            '*',
                                                                            newrec_.line_no,
                                                                            newrec_.release_no),
                                   single_occ_attr_);
            Client_SYS.Add_To_Attr('COUNTRY_CODE',
                                   Pur_Order_Line_Address_API.Get_Country_Code(newrec_.order_no,
                                                                               '*',
                                                                               newrec_.line_no,
                                                                               newrec_.release_no),
                                   single_occ_attr_);
            Client_SYS.Add_To_Attr('ADDRESS1',
                                   Pur_Order_Line_Address_API.Get_Address1(newrec_.order_no,
                                                                           '*',
                                                                           newrec_.line_no,
                                                                           newrec_.release_no),
                                   single_occ_attr_);
            Client_SYS.Add_To_Attr('ADDRESS2',
                                   Pur_Order_Line_Address_API.Get_Address2(newrec_.order_no,
                                                                           '*',
                                                                           newrec_.line_no,
                                                                           newrec_.release_no),
                                   single_occ_attr_);
            Client_SYS.Add_To_Attr('ZIP_CODE',
                                   Pur_Order_Line_Address_API.Get_Zip_Code(newrec_.order_no,
                                                                           '*',
                                                                           newrec_.line_no,
                                                                           newrec_.release_no),
                                   single_occ_attr_);
            Client_SYS.Add_To_Attr('CITY',
                                   Pur_Order_Line_Address_API.Get_City(newrec_.order_no,
                                                                       '*',
                                                                       newrec_.line_no,
                                                                       newrec_.release_no),
                                   single_occ_attr_);
            Client_SYS.Add_To_Attr('ADDR_STATE',
                                   Pur_Order_Line_Address_API.Get_Addr_State(newrec_.order_no,
                                                                             '*',
                                                                             newrec_.line_no,
                                                                             newrec_.release_no),
                                   single_occ_attr_);
            Client_SYS.Add_To_Attr('COUNTY',
                                   Pur_Order_Line_Address_API.Get_County(newrec_.order_no,
                                                                         '*',
                                                                         newrec_.line_no,
                                                                         newrec_.release_no),
                                   single_occ_attr_);

            Pur_Order_Line_Address_API.New(single_occ_attr_);
         END IF;

         IF NOT
            (Pur_Ord_Charged_Comp_API.Check_Any_Exist(destination_order_,
                                                      newrec_.line_no,
                                                      newrec_.release_no) = 1) THEN
            --Note: Copy any Supplier Material for the old PO Line to the new PO Line
            Pur_Ord_Charged_Comp_API.Copy(order_no_,
                                          line_no_,
                                          release_no_,
                                          destination_order_,
                                          newrec_.line_no,
                                          newrec_.release_no);
         END IF;
         --Bug 25648, start
         IF NOT
            (Purchase_Order_Line_Comp_API.Check_Any_Exist(destination_order_,
                                                          newrec_.line_no,
                                                          newrec_.release_no)) THEN
            --Note: Copy any Supplier Material for the old PO Line to the new PO Line
            Purchase_Order_Line_Comp_API.Copy(order_no_,
                                              line_no_,
                                              release_no_,
                                              destination_order_,
                                              newrec_.line_no,
                                              newrec_.release_no);
         END IF;
         --Bug 25648, end
         newrec_.note_id := Client_SYS.Get_Item_Value('NOTE_ID', attr_);
         IF (include_doc_texts_ = 'Y') THEN
            Document_Text_API.Copy_All_Note_Texts(org_note_id_,
                                                  newrec_.note_id);
         END IF;

         org_pre_accounting_id_  := newrec_.pre_accounting_id;
         temp_pre_accounting_id_ := Client_SYS.Get_Item_Value('PRE_ACCOUNTING_ID',
                                                              attr_);
         head_pre_accounting_id_ := purchase_order_rec_.pre_accounting_id;

         header_company_ := Site_API.Get_Company(Purchase_Order_API.Get_Contract(newrec_.order_no));
         line_company_   := Site_API.Get_Company(newrec_.contract);

         IF (Purchase_Order_Line_Util_API.Is_Equal(header_company_,
                                                   line_company_) = 1) THEN
            IF (include_pre_accounting_ = 'Y') THEN
               Pre_Accounting_API.Copy_Pre_Accounting(org_pre_accounting_id_,
                                                      temp_pre_accounting_id_,
                                                      newrec_.contract,
                                                      TRUE);
            END IF;
         END IF;

         hist_msg_ := Language_SYS.Translate_Constant(lu_name_,
                                                      'ORDCOPY: Copied from order no :P1',
                                                      NULL,
                                                      order_no_);
         Purchase_Order_Line_Hist_API.Insert_History(destination_order_,
                                                     newrec_.line_no,
                                                     newrec_.release_no,
                                                     '*',
                                                     Fnd_Session_API.Get_Fnd_User,
                                                     hist_msg_,
                                                     '',
                                                     '',
                                                     '');
         line_no_    := newrec_.line_no;
         release_no_ := newrec_.release_no;
      ELSE
         Error_SYS.Record_General(lu_name_,
                                  'ORDNOTCOPIED: Part No :P1 is not connected to the Supplier :P2. The Order will not be copied.',
                                  newrec_.part_no,
                                  new_supplier2_);
      END IF;
   END IF;
   CLOSE SOURCE;

END Split_Line;


FUNCTION Config_Part_Line_Connected(order_no_   IN VARCHAR2,
                                    line_no_    IN VARCHAR2,
                                    release_no_ IN VARCHAR2) RETURN NUMBER IS
   CURSOR part_conected IS
    SELECT 1
      FROM PURCHASE_ORDER_LINE_PART
     WHERE configuration_id <> '*'
       AND order_no = order_no_
       AND line_no = line_no_
       AND release_no = release_no_;
   connected_ NUMBER;
BEGIN
   OPEN part_conected;
   FETCH part_conected
    INTO connected_;
   IF (part_conected%FOUND) THEN
      CLOSE part_conected;
      RETURN connected_;
   ELSE
      connected_ := 0;
   END IF;
   CLOSE part_conected;
   RETURN connected_;
END Config_Part_Line_Connected;

PROCEDURE SplitWithPrefix__(info_                   IN OUT VARCHAR2,
                            destination_order_      IN OUT VARCHAR2,
                            new_delivery_date_      IN OUT DATE,
                            order_no_               IN     VARCHAR2,
                            line_no_                IN     VARCHAR2,
                            release_no_             IN     VARCHAR2,
                            new_supplier_           IN     VARCHAR2,
                            new_blanket_            IN     VARCHAR2,
                            include_doc_texts_      IN     VARCHAR2,
                            include_internal_notes_ IN     VARCHAR2,
                            include_pre_accounting_ IN     VARCHAR2,
                            check_existing_         IN     VARCHAR2,
                            nprefix_                IN     VARCHAR2) IS
BEGIN
   -- Karam
   Purchase_Order_API.GPrefix_ := nprefix_;
   Purchase_Order_API.GPOQty_  := NULL;

   Split__(info_,
           destination_order_,
           new_delivery_date_,
           order_no_,
           line_no_,
           release_no_,
           new_supplier_,
           new_blanket_,
           include_doc_texts_,
           include_internal_notes_,
           include_pre_accounting_,
           check_existing_);

   Purchase_Order_API.GPrefix_ := NULL;
   Purchase_Order_API.Prefix_  := NULL;

END SplitWithPrefix__;

--HAL10003,End

PROCEDURE InitTenderParam IS
BEGIN
   Purchase_Order_API.GDoc_No_ := NULL;
   Purchase_Order_API.GTender_No_ := NULL;
   Purchase_Order_API.GPrefix_ := NULL;
   Purchase_Order_API.GPoQty_ := NULL;
   Purchase_Order_API.GDelDate_ := NULL;
   Purchase_Order_API.GBidSno_ := NULL;
END InitTenderParam;

PROCEDURE Tender_Req_Line_To_Multi_Order(req_order_no_          IN OUT VARCHAR2,
                                         assg_line_no_          IN OUT VARCHAR2,
                                         assg_release_no_       IN OUT VARCHAR2,
                                         req_info_              IN OUT VARCHAR2,
                                         tender_no_             IN     VARCHAR2,
                                         doc_no_                IN     VARCHAR2,
                                         bid_sno_               IN     NUMBER,
                                         requisition_no_        IN     VARCHAR2,
                                         line_no_               IN     VARCHAR2,
                                         release_no_            IN     VARCHAR2,
                                         authorize_code_        IN     VARCHAR2,
                                         buyer_code_            IN     VARCHAR2,
                                         new_order_             IN     VARCHAR2 DEFAULT NULL,
                                         use_default_buyer_     IN     VARCHAR2 DEFAULT NULL,
                                         prefix_                IN     VARCHAR2 DEFAULT NULL,
                                         purchase_site_         IN     VARCHAR2 DEFAULT NULL,
                                         central_order_flag_    IN     VARCHAR2 DEFAULT 'NOT CENTRAL ORDER',
                                         check_approval_amount_ IN     VARCHAR2 DEFAULT 'TRUE',
                                         orderqty_              IN     NUMBER DEFAULT NULL,
                                         suppliercontact_       IN     VARCHAR2 DEFAULT NULL,
                                         inquiry_no_            IN     NUMBER DEFAULT NULL,
                                         inquiry_line_          IN     NUMBER DEFAULT NULL) IS
   podate_    DATE;
   pordqty_   NUMBER;
   podeldate_ DATE;
   ordqty_    NUMBER := orderqty_;

   mdoc_no_    VARCHAR2(50) := doc_no_;
   mtender_no_ VARCHAR2(50) := tender_no_;
   mprefix_    VARCHAR2(50) := prefix_;
   mbid_sno_   NUMBER := bid_sno_;

   get_req_pur_req_line_ Purchase_Req_Line_API.Public_Rec;
   --(+)<090727><AnRaIn> <<C_CENT_TENDER_PROCESS-1>><start>
   contract_ site_tab.contract%TYPE;
   --(+)<090727><AnRaIn> <<C_CENT_TENDER_PROCESS-1>><end>
   --nagain 230927
   split_qty_ number;
   --nagain 230927
   BEGIN
      --nagain 230927
      split_qty_ := TENDER_BIDS_DETAIL_api.Get_C_Split_Quantity (doc_no_ ,  tender_no_  ,requisition_no_ , line_no_ , release_no_ );
   --nagain 230927
   
   DBMS_OUTPUT.PUT_LINE('=======Tender_Req_Line_To_Multi_Order (Begin)======');
   DBMS_OUTPUT.PUT_LINE('Tender_No --> ' || tender_no_);
   DBMS_OUTPUT.PUT_LINE('Doc_No    --> ' || doc_no_);

   IF Purchase_Order_API.GMultipleRow_ = 'TRUE' THEN
      DBMS_OUTPUT.PUT_LINE('GMultiRow    --> TRUE');
      get_req_pur_req_line_ := Purchase_Req_Line_API.Get(requisition_no_, line_no_,release_no_);
      Purchase_Order_API.GPoQty_ := Requisition_Order_Details_API.Get_Supplier_BalQty(doc_no_,
                                                                                      tender_no_,
                                                                                      requisition_no_,
                                                                                      line_no_,
                                                                                      release_no_);
   ELSE
      DBMS_OUTPUT.PUT_LINE('GMultiRow    --> FALSE');
      ordqty_ := Requisition_Order_Details_API.Get_Purch_Conv_Qty(doc_no_,
                                                                  tender_no_,
                                                                  requisition_no_,
                                                                  line_no_,
                                                                  release_no_,
                                                                  orderqty_,
                                                                  'TUOM',
                                                                  'PUOM');
      DBMS_OUTPUT.PUT_LINE('Order Qty (Supplier) = ' || ordqty_);
      Purchase_Order_API.GPoQty_ := ordqty_;
   END IF;
   ordqty_ := Purchase_Order_API.GPoQty_;
   DBMS_OUTPUT.PUT_LINE('Quantity --> ' || ordqty_);
   Purchase_Order_API.GMultipleRow_ := 'FALSE';
   --(-/+)<090727><AnRaIn> <<C_CENT_TENDER_PROCESS-1>><start>
   contract_ := purchase_site_;
   Tender_Bid_Delivery_Detail_API.GetOrderQty(doc_no_,
                                              tender_no_,
                                              requisition_no_,
                                              line_no_,
                                              release_no_,
                                              podate_,
                                              ordqty_,
                                              pordqty_,
                                              podeldate_,
                                              contract_);
   --(-/+)<090727><AnRaIn> <<C_CENT_TENDER_PROCESS-1>><end>
   IF Purchase_Order_API.GMultipleRow_ = 'FALSE' AND ordqty_ > 0 THEN
      ordqty_ := Requisition_Order_Details_API.Get_Purch_Conv_Qty(doc_no_,
                                                                  tender_no_,
                                                                  requisition_no_,
                                                                  line_no_,
                                                                  release_no_,
                                                                  ordqty_,
                                                                  'PUOM',
                                                                  'TUOM');
   END IF;
   IF Purchase_Order_API.GTender_No_ IS NULL THEN
      Purchase_Order_API.GTender_No_ := tender_no_;
   END IF;
   IF Purchase_Order_API.GDoc_No_ IS NULL THEN
      Purchase_Order_API.GDoc_No_ := doc_no_;
   END IF;

   Purchase_Order_API.GPrefix_ := Prefix_;
   Purchase_Order_API.GPOQty_ := pordqty_;
   IF podeldate_ IS NULL THEN
      podeldate_ := Tender_Bids_Detail_API.GET_DELIVERY_DATE(doc_no_,
                                                             tender_no_,
                                                             requisition_no_,
                                                             line_no_,
                                                             release_no_);
   END IF;
   Trace_SYS.Field('PO Delv Date', podeldate_);

   IF req_order_no_ IS NOT NULL THEN
      podate_ := Purchase_Order_API.GET_ORDER_DATE(req_order_no_);
   ELSE
      podate_ := Nvl(podeldate_, sysdate);
   END IF;
   Purchase_Order_API.GDelDate_ := podeldate_;
   Purchase_Order_API.GBidSno_ := bid_sno_;
   IF Nvl(pordqty_, 0) > 0 THEN
      Dbms_Output.Put_Line('--------------------------------------------------------------------');
      Trace_SYS.Field('<<<<<<< Tender No ', tender_no_ || '>>>>>>');
      Trace_SYS.Field('<<<<<<< Doc No ', doc_no_ || '>>>>>>');
      Trace_SYS.Field('<<<<<<< Order Qty ', to_char(pordqty_) || '>>>>>>');
      Trace_SYS.Field('<<<<<<< Order Del Date ',
                      to_char(podeldate_, 'DD/MM/YYYY') || '>>>>>>');
      Trace_SYS.Field('<<<<<<< Bid Sno ', to_char(bid_sno_) || '>>>>>>');
      Dbms_Output.Put_Line('--------------------------------------------------------------------');
      
      IF nvl(split_qty_,0) != 0 THEN
         pordqty_ := split_qty_;
      END IF;

      User_Tender_Req_Line_To_Order(req_order_no_,
                                    assg_line_no_,
                                    assg_release_no_,
                                    req_info_,
                                    tender_no_,
                                    doc_no_,
                                    requisition_no_,
                                    line_no_,
                                    release_no_,
                                    authorize_code_,
                                    buyer_code_,
                                    new_order_,
                                    use_default_buyer_,
                                    Prefix_,
                                    nvl(contract_, purchase_site_), ---- --(+)<090727><AnRaIn> <<C_CENT_TENDER_PROCESS-1>>
                                    central_order_flag_,
                                    NULL, --- C_G1386311-1 Tender
                                    pordqty_,
                                    suppliercontact_,
                                    inquiry_no_,
                                    inquiry_line_);
   END IF;

   IF Nvl(ordqty_, 0) <= 0 THEN
      Purchase_Order_API.GDoc_No_ := NULL;
      Purchase_Order_API.GTender_No_ := NULL;
      Purchase_Order_API.GPrefix_ := NULL;
      Purchase_Order_API.GPoQty_ := NULL;
      Purchase_Order_API.GDelDate_ := NULL;
      Purchase_Order_API.GBidSno_ := NULL;
   ELSE
      Tender_Req_Line_To_Multi_Order(req_order_no_,
                                     assg_line_no_,
                                     assg_release_no_,
                                     req_info_,
                                     mtender_no_,
                                     mdoc_no_,
                                     mbid_sno_,
                                     requisition_no_,
                                     line_no_,
                                     release_no_,
                                     authorize_code_,
                                     buyer_code_,
                                     new_order_,
                                     use_default_buyer_,
                                     mprefix_,
                                     purchase_site_,
                                     central_order_flag_,
                                     ordqty_);
   END IF;

   DBMS_OUTPUT.PUT_LINE('Prefix :: ' || Purchase_Order_API.Gprefix_);
   DBMS_OUTPUT.PUT_LINE('=======Tender_Req_Line_To_Order (End)======');
END Tender_Req_Line_To_Multi_Order;

PROCEDURE User_Tender_Req_Line_To_Order(req_order_no_          IN OUT VARCHAR2,
                                        assg_line_no_          IN OUT VARCHAR2,
                                        assg_release_no_       IN OUT VARCHAR2,
                                        req_info_              IN OUT VARCHAR2,
                                        tender_no_             IN     VARCHAR2,
                                        doc_no_                IN     VARCHAR2,
                                        requisition_no_        IN     VARCHAR2,
                                        line_no_               IN     VARCHAR2,
                                        release_no_            IN     VARCHAR2,
                                        authorize_code_        IN     VARCHAR2,
                                        buyer_code_            IN     VARCHAR2,
                                        new_order_             IN     VARCHAR2 DEFAULT NULL,
                                        use_default_buyer_     IN     VARCHAR2 DEFAULT NULL,
                                        prefix_                IN     VARCHAR2 DEFAULT NULL,
                                        purchase_site_         IN     VARCHAR2 DEFAULT NULL,
                                        central_order_flag_    IN     VARCHAR2 DEFAULT 'NOT CENTRAL ORDER',
                                        check_approval_amount_ IN     VARCHAR2 DEFAULT 'TRUE',
                                        orderqty_              IN     NUMBER DEFAULT NULL,
                                        suppliercontact_       IN     VARCHAR2 DEFAULT NULL,
                                        inquiry_no_            IN     NUMBER DEFAULT NULL,
                                        inquiry_line_          IN     NUMBER DEFAULT NULL) 
IS
   --ENVNAGANB 010923 <DML Code Correction> (START)
      CURSOR get_objid_(v_order_no_ VARCHAR2,v_line_no VARCHAR2,v_release_no VARCHAR2)IS 
       SELECT objid,Objversion
       FROM PURCHASE_ORDER_LINE
       WHERE ORDER_NO = v_order_no_
       AND line_no = v_line_no
       AND release_no = v_release_no;
       
   objid_      VARCHAR2(2000);
   objversion_ VARCHAR2(2000);
   attr_  VARCHAR2(32000);
   info_  VARCHAR2(32000);
      --nagain 230927
   split_qty_ number;
   --nagain 230927
   --ENVNAGANB 010923 <DML Code Correction> (FINISH)
BEGIN
   --nagain 230927
      split_qty_ := TENDER_BIDS_DETAIL_api.Get_C_Split_Quantity (doc_no_ ,  tender_no_  ,requisition_no_ , line_no_ , release_no_ );
   --nagain 230927
   DBMS_OUTPUT.PUT_LINE('=======User_Tender_Req_Line_To_Order (Begin)======');
   DBMS_OUTPUT.PUT_LINE('Tender_No --> ' || tender_no_);
   DBMS_OUTPUT.PUT_LINE('Doc_No    --> ' || doc_no_);
   Purchase_Order_API.GMultipleRow_ := 'FALSE';

   Purchase_Order_API.GTender_No_ := tender_no_;
   Purchase_Order_API.GDoc_No_ := doc_no_;
   Purchase_Order_API.GPrefix_ := prefix_;
   Purchase_Order_API.GPOQty_ := orderqty_;
--nagain 230927
   if nvl(split_qty_,0) != 0 then 
        Purchase_Order_API.GPOQty_ := split_qty_;
      else 
         Purchase_Order_API.GPOQty_ := orderqty_;
         split_qty_ := orderqty_;
      end if;
--nagain 230927
   User_Requisition_Line_To_Order(req_order_no_,
                            assg_line_no_,
                            assg_release_no_,
                            req_info_,
                            requisition_no_,
                            line_no_,
                            release_no_,
                            authorize_code_,
                            buyer_code_,
                            new_order_,
                            use_default_buyer_,
                            purchase_site_,
                            central_order_flag_,
                            'TRUE',
                            inquiry_no_,
                            inquiry_line_,
                            NULL,
							NULL, 
							split_qty_);

   IF req_order_no_ IS NOT NULL THEN
     Purchase_Order_Line_API.Modify_GST_Info(order_no_                  => req_order_no_, 
                                                line_no_                => assg_line_no_, 
                                                release_no_             => assg_release_no_, 
                                                unit_manual_base_amt_   => TENDER_BIDS_DETAIL_API.Get_Unit_Manual_Base_Amount(doc_no_,tender_no_,requisition_no_,line_no_,release_no_),
                                                deliv_type_id_          => TENDER_BIDS_DETAIL_API.Get_Deliv_Type_Id(doc_no_,tender_no_,requisition_no_,line_no_,release_no_),  
                                                hsn_sac_code_           => TENDER_BIDS_DETAIL_API.Get_C_Hsn_Sac_Code(doc_no_,tender_no_,requisition_no_,line_no_,release_no_),  
                                                hsn_sac_type_db_        => TENDER_BIDS_DETAIL_API.Get_C_Hsn_Sac_Type_Db(doc_no_,tender_no_,requisition_no_,line_no_,release_no_),  
                                                gst_applicable_db_      => TENDER_BIDS_DETAIL_API.Get_C_Gst_Applicable_Db(doc_no_,tender_no_,requisition_no_,line_no_,release_no_),   
                                                gst_exemption_reason_   => TENDER_BIDS_DETAIL_API.Get_C_Gst_Exemption_Reason(doc_no_,tender_no_,requisition_no_,line_no_,release_no_),  
                                                pos_                    => TENDER_BIDS_DETAIL_API.Get_C_Pos(doc_no_,tender_no_,requisition_no_,line_no_,release_no_));
  --ENVNAGANB 010923 <DML Code Correction> (START)
       OPEN get_objid_(req_order_no_,assg_line_no_,assg_release_no_);
       FETCH get_objid_ INTO Objid_,Objversion_;
       CLOSE get_objid_;
       
      Client_SYS.Clear_Attr(attr_);
      Client_SYS.Add_To_Attr('CONV_FACTOR', Nvl(Tender_bids_detail_API.Get_PURCH_CONV_FACTOR(doc_no_,
                                                                          tender_no_,
                                                                          requisition_no_,
                                                                          line_no_,
                                                                          release_no_),
                             1), attr_);
     IF objid_ IS NOT NULL AND objversion_ IS NOT NULL THEN
         Purchase_Order_Line_API.Modify__(info_, objid_, objversion_, attr_, 'DO');
     END IF;
  --ENVNAGANB 010923 <DML Code Correction> (FINISH)

      Purch_Struc_Insertion_API.Delete_Structure(req_order_no_,
                                                 assg_line_no_,
                                                 assg_release_no_);

      Purch_Struc_Insertion_API.POST_TENDER_STRUC_TO_PO(Tender_Header_API.Get_Contract(tender_no_),
                                                        doc_no_,
                                                        tender_no_,
                                                        requisition_no_,
                                                        line_no_,
                                                        release_no_,
                                                        req_order_no_,
                                                        assg_line_no_,
                                                        assg_release_no_,
                                                        Nvl(Tender_Detail_API.Get_Part_No(tender_no_,
                                                                                             requisition_no_,
                                                                                             line_no_,
                                                                                             release_no_),
                                                            'NO PART'),
                                                        Purchase_Order_API.Get_Vendor_No(req_order_no_),
                                                        Tender_Bids_API.Get_Strcode(doc_no_),
                                                        Purchase_Order_API.GET_ORDER_DATE(req_order_no_),
                                                        'TENDER');

      Tender_Detail_API.Update_PO_Created(tender_no_,
                                          requisition_no_,
                                          line_no_,
                                          release_no_);

   END IF;

   Purchase_Order_API.GDoc_No_ := NULL;
   Purchase_Order_API.GTender_No_ := NULL;
   Purchase_Order_API.GPrefix_ := NULL;
   Purchase_Order_API.GPoQty_ := NULL;

END User_Tender_Req_Line_To_Order;

--<start> <C_Pending Issues for Closure -PNC-1>
PROCEDURE Check_Dept_Access(order_no_ IN VARCHAR2, site_ IN VARCHAR2) IS
   user_id_      VARCHAR2(30);
   person_id_    VARCHAR2(20);
   emp_id_       VARCHAR2(11);
   org_code_     VARCHAR2(10);
   fnd_user_org_ VARCHAR2(10);
   stmt_         VARCHAR2(2000);
   CURSOR get_user_id IS
    SELECT t.userid
      FROM PURCHASE_ORDER_HIST t
     WHERE t.order_no = order_no_
       AND t.hist_state = 'Planned'
     ORDER BY t.date_entered DESC;

BEGIN
   OPEN get_user_id;
   FETCH get_user_id
   INTO user_id_;
   CLOSE get_user_id;
   person_id_ := Person_Info_API.Get_Id_For_User(user_id_);

   $IF Component_Percos_SYS.INSTALLED $THEN
      emp_id_ := Company_Person_API.Get_Person_Emp_No(Site_API.Get_Company(site_),person_id_);
   $END

   stmt_ := NULL;
   $IF Component_Percos_SYS.INSTALLED $THEN
      org_code_ := Company_Pers_Assign_API.Get_Org_Code(Site_API.Get_Company(site_),emp_id_,TRUNC(sysdate));
   $END

   person_id_ := NULL;
   emp_id_    := NULL;

   person_id_ := Person_Info_API.Get_Id_For_User(Fnd_Session_API.Get_Fnd_User);

   stmt_ := NULL;

   $IF Component_Percos_SYS.INSTALLED $THEN
      emp_id_ := Company_Person_API.Get_Person_Emp_No(Site_API.Get_Company(site_),person_id_);
   $END

   --fnd_user_org_ := Company_Pers_Assign_API.Get_Org_Code(site_api.Get_Company(site_),emp_id_,TRUNC(SYSDATE));
   $IF Component_Percos_SYS.INSTALLED $THEN
      org_code_ := Company_Pers_Assign_API.Get_Org_Code(Site_API.Get_Company(site_),emp_id_,TRUNC(sysdate));
   $END

   IF nvl(org_code_, 'null_org') <> nvl(fnd_user_org_, 'null_fnd_org') THEN
      Error_SYS.Record_General(lu_name_,'DEPTERR: Modification Denied, Only (:P1) Department''s user can modify the Purchase Order', org_code_);
   END IF;
END Check_Dept_Access;


FUNCTION Get_Revision_Open(order_no_ IN VARCHAR2) RETURN VARCHAR2
IS
BEGIN
	IF Get_Objstate(order_no_) = 'Planned' AND Get_Revision(order_no_) > 1 THEN
		 RETURN 'TRUE';
	ELSE
		 RETURN 'FALSE';
	END IF;
END Get_Revision_Open;


PROCEDURE Copy_With_Prefix_ (
   info_                   IN OUT NOCOPY VARCHAR2,
   destination_order_      IN OUT NOCOPY VARCHAR2,
   new_delivery_date_      IN OUT NOCOPY DATE,
   order_no_               IN     VARCHAR2,
   new_supplier_           IN     VARCHAR2,
   new_blanket_            IN     VARCHAR2,
   include_doc_texts_      IN     VARCHAR2,
   include_internal_notes_ IN     VARCHAR2,
   include_pre_accounting_ IN     VARCHAR2,
   prefix_                 IN   VARCHAR2)
IS
BEGIN
   
    PURCHASE_ORDER_API.GPrefix_ := prefix_;
    PURCHASE_ORDER_API.GPOQty_  := Null;
    Copy__(info_ => info_,
           destination_order_ => destination_order_,
           new_delivery_date_ => new_delivery_date_,
           order_no_ => order_no_,
           new_supplier_ => new_supplier_,
           new_blanket_ => new_blanket_,
           include_doc_texts_ => include_doc_texts_,
           include_internal_notes_ => include_internal_notes_,
           include_pre_accounting_ => include_pre_accounting_ 
    );
    PURCHASE_ORDER_API.GPrefix_ := null;
    PURCHASE_ORDER_API.GPOQty_  := Null;
END  Copy_With_Prefix_;  
--(+) 231121 TechSurenS MERGE-ENV-ECON-1 (FINISH)


--(+)231114 TecVaruSa BLK-IMM-02-M (START)
PROCEDURE C_Short_Close_Reason_Update(
   order_no_ IN VARCHAR2,
   line_no_  IN VARCHAR2,
   rel_no_   IN VARCHAR2,
   mode_     IN VARCHAR2) 
IS
   info_         VARCHAR2(32000);
   attr_         VARCHAR2(32000);
   reason_       VARCHAR2(12);
   
   
   CURSOR get_po_lines_keys IS 
      SELECT pol.objid,
             pol.objversion
      FROM   purchase_order_line_part pol
      WHERE  pol.order_no                 = order_no_
      AND    nvl(pol.c_short_closed,'No') = 'No'
      AND    pol.objstate                 = 'Received';
   
   CURSOR get_po_line_keys IS 
      SELECT pol.objid,
             pol.objversion
      FROM   purchase_order_line_part pol
      WHERE  pol.order_no   = order_no_
      AND    pol.line_no    = line_no_
      AND    pol.release_no = rel_no_;  
      
BEGIN 
   IF mode_ = 'Header' THEN 
      IF nvl(Purchase_Order_API.Get_C_Short_Closed(order_no_),'No') = 'No' THEN 
         Error_SYS.Record_General(lu_name_,'CPOSHORTCLOSERR: Short Close Reasons is not updated.');
      END IF;
      
      reason_ := Purchase_Order_API.Get_C_Short_Close_Reason(order_no_);
      
      FOR lines_rec_ IN get_po_lines_keys LOOP 
         Client_SYS.Clear_Attr(attr_);
         Client_SYS.Add_To_Attr('C_SHORT_CLOSED',       'Yes',   attr_);
         Client_SYS.Add_To_Attr('C_SHORT_CLOSE_REASON', reason_, attr_);
         
         Purchase_Order_Line_Part_API.Modify__(info_, lines_rec_.objid, lines_rec_.objversion, attr_, 'DO');
      END LOOP; 
   ELSIF mode_ = 'Line' THEN 
      IF nvl(Purchase_Order_Line_Part_API.Get_C_Short_Closed(order_no_,line_no_,rel_no_),'No') = 'No' THEN 
         Error_SYS.Record_General(lu_name_,'CPOLSHORTCLOSERR: Short Close Reasons is not updated for the line.');
      END IF;
      
   ELSIF mode_ = 'NoPartLine' THEN    
      IF nvl(Purchase_Order_Line_Nopart_API.Get_C_Short_Closed(order_no_,line_no_,rel_no_),'No') = 'No' THEN 
         Error_SYS.Record_General(lu_name_,'CPOLSHORTCLOSERR: Short Close Reasons is not updated for the line.');
      END IF;
   ELSIF mode_ = 'Reopen' THEN 
      FOR line_rec_ IN get_po_line_keys LOOP
         Client_SYS.Clear_Attr(attr_);
         Client_SYS.Add_To_Attr('C_SHORT_CLOSED',       'No', attr_);
         Client_SYS.Add_To_Attr('C_SHORT_CLOSE_REASON', '',   attr_);

         Purchase_Order_Line_Part_API.Modify__(info_, line_rec_.objid, line_rec_.objversion, attr_, 'DO');
      END LOOP;
   END IF;
END C_Short_Close_Reason_Update;


FUNCTION C_Get_Associated_Buyer(
   order_no_ IN VARCHAR2) RETURN VARCHAR2
IS 
   CURSOR get_associated_buyer IS 
      SELECT ab.buyer_id
      FROM   person_info_tab         pi,
             c_associate_buyer_tab   ab,
             purchase_order_tab      po
      WHERE  pi.person_id  = ab.buyer_id
      AND    po.order_no   = order_no_
      AND    pi.user_id    = fnd_session_api.get_fnd_user
      AND    ab.person_id  = po.buyer_code;
   
   buyer_id_ VARCHAR2(20);
BEGIN 
   OPEN  get_associated_buyer;
   FETCH get_associated_buyer INTO buyer_id_;
   CLOSE get_associated_buyer;
   
   IF buyer_id_ IS NOT NULL THEN 
      RETURN 'TRUE';
   ELSE 
      RETURN 'FALSE';
   END IF;
END C_Get_Associated_Buyer;
--(+)231114 TecVaruSa BLK-IMM-02-M (FINISH)

FUNCTION C_Check_So_Demand_line(
   order_no_ IN VARCHAR2) RETURN NUMBER
IS 
   
   CURSOR get_line_count IS 
   SELECT COUNT(*)
   FROM   purchase_order_line_tab p
   WHERE  p.order_no    = order_no_
   AND    p.demand_code = 'SO';
   
   count_ NUMBER;
BEGIN 
   OPEN  get_line_count;
   FETCH get_line_count INTO count_;
   CLOSE get_line_count;
   
   RETURN count_;
END C_Check_So_Demand_line;
