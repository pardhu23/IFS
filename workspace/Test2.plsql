-----------------------------------------------------------------------------
--
--  Logical unit: CustomerOrder
--  Component:    ORDER
--
--  IFS Developer Studio Template Version 3.0
--
--  Date    Sign    History
--  ------  ------  ---------------------------------------------------------
-- 231223   TecKiranK    M-CSS-6.4.1-001   : Removed code in Finite_State_Set___()
-- 231129   SB00812057   M-11.3.5-BR001    : Modified Set_Released__
-- 231129   TecPrayas    C-CSS-6.4.1-001A  : Modified Check_Update___ and created Insert___
-- 231121   TecVaruSa    MERGE-ENVECON-1   : Merged Envecon Changes
-- 231030   TecGovinT    M-CSS-082         : Modified
-- 231011   TecGovinT    M-CSS-082         : Modified
-- 231004   TecGovinT    M-CSS-134_141_142 : Modified Check_Common___ Method
-- 230928   TechPriyaD   C-CSS-6.4.1-001   : Validation for Order Receipt Date & Customer PO Date.
-- 230911   TecFarahS    M-11.3.5-BR001    : Close the All Shop Order once the Customer Order Invoiced
-- 230901   TecFarahS    M-2.8.2-BR001     : Added validation for Invoice Customer which is not planned 
-- 230823   TecKarunkarS M-CSS-134_141_142 : Modified - added Add_Param_Warranty and Add_Lot_Batch_Warranty
-- 230720   TecGovinT    M-CSS-082         : Modified
-- 230613   TecGovinT    M-CSS-6.8.9-001   : Modified - added functiOnality for pegging
-- 230215   TecKiranK    M-CSS-6.4.1-001   : Modified Finite_State_Set___()
-- 221112   TecKarunkarS M-CSS-013A        : Added Condition to Set_Released order_line_total_base,order_line_total_curr,order_line_gross_total_curr
-- 230419   TecKrishna   M-2.8.2-BR001     : Added condition to 'Check_Common___' procedure to raise the message
-----------------------------------------------------------------------------

layer Cust;

-------------------- PUBLIC DECLARATIONS ------------------------------------


-------------------- PRIVATE DECLARATIONS -----------------------------------
--(+) 231121 TecVaruSa MERGE-ENVECON-1 (START)
@ApproveGlobalVariable(2016/04/18, kasoin)
prefix_			         VARCHAR2(20);  
@ApproveGlobalVariable(2017/08/30, kasoin)
GAdvOnHsn_        VARCHAR2(5) := 'FALSE';
--(+) 231121 TecVaruSa MERGE-ENVECON-1 (FINISH)

-------------------- LU SPECIFIC IMPLEMENTATION METHODS ---------------------
--(+) 231121 TecVaruSa MERGE-ENVECON-1 (START)
@Override
PROCEDURE Prepare_Insert___ (
   attr_ IN OUT VARCHAR2 )
IS
BEGIN
   --Add pre-processing code here
   super(attr_);
   Client_SYS.Set_Item_Value('AUTHORIZE_STATUS','Not Required',attr_);
   Client_SYS.Set_Item_Value('TRADING_ORDER','FALSE',attr_);
   Client_SYS.Add_To_Attr('FREIGHT_FLAG', 'NotApplicable', attr_);
END Prepare_Insert___;

@Override
PROCEDURE Check_Insert___ (
   newrec_ IN OUT customer_order_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
   company_               VARCHAR2(20);
   flag_           VARCHAR2(5);
BEGIN
   prefix_ := Client_SYS.Get_Item_Value('PREFIX',attr_);
   --Add pre-processing code here
   super(newrec_, indrec_, attr_);
   IF newrec_.contract IS NOT NULL THEN
      company_ := Site_API.Get_Company(newrec_.contract);
   END IF;      
   IF (newrec_.form_type IS NOT NULL) THEN
      Comp_Form_Type_API.Exist(company_, newrec_.form_type);
   END IF;
   IF (newrec_.form_rate_type IS NOT NULL) THEN
      Comp_Form_Type_Line_API.Check_Form_Rate_Type(company_, newrec_.form_type, 0, newrec_.form_rate_type);
   END IF;
   IF  newrec_.customer_po_date > sysdate THEN
      Error_SYS.Record_General(lu_name_, 'PODATEGREATER: Customer PO Date can not be later than Sysdate.');
   END IF;
   IF (newrec_.payment_method IS NOT NULL) THEN
      $IF Component_Payled_SYS.INSTALLED $THEN
		 	Payment_Way_API.Exist(company_, newrec_.payment_method);
      $ELSE
         NULL;
      $END
   END IF;
   IF (newrec_.discounting_bank IS NOT NULL) THEN
      $IF Component_Payled_SYS.INSTALLED $THEN
		 	Payment_Institute_API.Exist(company_, newrec_.discounting_bank);
      $ELSE
         NULL;
      $END
   END IF;
   IF newrec_.freight_flag IS NULL THEN
      newrec_.freight_flag := 'N';
   END IF;
   User_Access_Depots_API.Set_Depot_Access_Rights;
   User_Access_Depots_API.User_Depots_Access_Rights(Site_API.Get_Company(newrec_.contract),newrec_.customer_no,NULL,'I');
   IF (Customer_Info_Address_API.get_state(newrec_.customer_no,newrec_.ship_addr_no)) != (Site_Address_API.get_state_code(newrec_.contract)) THEN
      flag_ :=  Comp_Form_Type_API.get_inter_state(company_,newrec_.form_type);
      IF flag_ = 'FALSE' THEN
         Error_SYS.Record_Exist(lu_name_, 'NOINTERSTATE:  Not a Valid Inter State Form Type');
      END IF;
   ELSE
      flag_ :=  Comp_Form_Type_API.get_inter_state(company_,newrec_.form_type);
      IF flag_ = 'TRUE'  THEN
         Error_SYS.Record_Exist(lu_name_, 'NOINTERSTATE:  Not a Valid Intra State Form Type');
      END IF;
   END IF;
END Check_Insert___;

@Override
PROCEDURE Insert___ (
   objid_      OUT    VARCHAR2,
   objversion_ OUT    VARCHAR2,
   newrec_     IN OUT CUSTOMER_ORDER_TAB%ROWTYPE,
   attr_       IN OUT VARCHAR2 )
IS
   company_       VARCHAR2(20);
   prefix_no_ 		NUMBER;	
   identity_        VARCHAR2(20);
   order_date_      DATE;
   CURSOR get_identity 
   IS
      SELECT company_identity 
      FROM comp_struct
      WHERE company=company_;
BEGIN
   company_ := Site_API.Get_Company(newrec_.contract);
   IF newrec_.order_no IS NULL THEN
      --  090401  AjShIn  C_Fresh developments-1 Finish
	   IF Company_Tax_Info_API.Get_Order_Prefix_Flag(company_) = 'TRUE' THEN
         IF nvl(prefix_,'m') = 'm' THEN
            prefix_ := Site_Prefix_API.get_default_prefix(Site_Site_Group_API.get_site_group(newrec_.contract,'CO'),'CO');
         END IF;
         dbms_output.put_line('prefix_ '|| prefix_);
         Site_Prefix_API.valid_prefix_contract(newrec_.contract,prefix_,'CO');

         Site_Prefix_API.Exist(prefix_);
         order_date_ :=   Site_API.Get_Site_Date(newrec_.contract);
         Site_Prefix_API.Valid_Prefix(prefix_, order_date_ , 'CO');
         Site_Prefix_API.Get_Next_No(prefix_,prefix_no_);
         newrec_.order_no := prefix_||prefix_no_;
         Prefix_Transaction_API.ins_prefix_txn(newrec_.order_no,
                                               'CO',
                                               prefix_,
                                               prefix_||prefix_no_);
         Client_SYS.Add_To_Attr('ORDER_NO', newrec_.order_no, attr_);
	   END IF;
   END IF;   
   --(+) 231129  TecPrayas  C-CSS-6.4.1-001A(START)
   IF UPPER(newrec_.c_firm_order) = 'TRUE'  THEN
    newrec_.c_login_user := fnd_session_api.Get_Fnd_User;
    newrec_.c_time_cap   := sysdate;
    Client_SYS.Add_To_Attr('C_LOGIN_USER',fnd_session_api.Get_Fnd_User,attr_);
    Client_SYS.Add_To_Attr('C_TIME_CAP',  SYSDATE,                     attr_);
   END IF;
   --(+) 231129  TecPrayas  C-CSS-6.4.1-001A(FINISH)
   super(objid_, objversion_, newrec_, attr_);
   OPEN get_identity;
   FETCH get_identity INTO identity_;
   CLOSE  get_identity;
   IF identity_='Primary' THEN
      IF  Customer_Sales_Ananlysis_API.GET_SALES_ANALYSIS_FLAG(newrec_.customer_no)='TRUE'
         AND Site_Tax_Info_API.GET_SALES_ANALYSIS_FLAG(newrec_.contract)='TRUE' THEN
         Customer_Order_API.Insert_Struct_Cust(newrec_.order_no,company_,newrec_.customer_no);
      END IF;
   ELSIF identity_='Secondary' THEN
      IF  Site_Tax_Info_API.GET_SALES_ANALYSIS_FLAG(newrec_.contract)='TRUE' THEN
         Customer_Order_API.Insert_Struct_Site(newrec_.order_no,newrec_.contract,company_);
      END IF;
   END IF;
END Insert___;
--(+) 231121 TecVaruSa MERGE-ENVECON-1 (FINISH)

@Override
PROCEDURE Check_Common___ (
   oldrec_ IN            customer_order_tab%ROWTYPE,
   newrec_ IN OUT NOCOPY customer_order_tab%ROWTYPE,
   indrec_ IN OUT NOCOPY Indicator_Rec,
   attr_   IN OUT NOCOPY VARCHAR2 )
IS
   
BEGIN
   --(+)230419   TecKrishna   M-2.8.2-BR001 (START)   
   IF (newrec_.order_id = 'PRT' AND newrec_.rowstate= 'Planned' AND newrec_.customer_no_pay IS NULL) THEN
      Error_SYS.Record_General(lu_name_,'INVCUSTERR: Invoicing Customer is Mandatory for Participation Sales order types.');
   END IF;
   --(+)230419   TecKrishna   M-2.8.2-BR001 (END)
   
   --(+) 230928 TechPriyaD C-CSS-6.4.1-001(START)
   IF newrec_.c_cust_po_receipt_date > SYSDATE THEN
      Error_SYS.Record_General(lu_name_,'ORDERRECEIPTDATEERR: Order Receipt Date can not be future date' );
   END IF;
   
   IF newrec_.c_customer_po_date > SYSDATE THEN
      Error_SYS.Record_General(lu_name_,'CUSTOMERPODATEERR: Customer PO Date can not be future date' );
   END IF;
   --(+) 230928 TechPriyaD C-CSS-6.4.1-001(FINISH)
   
   --(+)231004 TecGovinT M-CSS-134_141_142 (START)
   IF newrec_.c_product IS NULL THEN
      newrec_.c_product := 'Not Applicable';
   END IF;
   IF newrec_.c_model IS NULL THEN
      newrec_.c_model   := 'Not Applicable';
   END IF;
   --(+)231004 TecGovinT M-CSS-134_141_142 (FINISH)
   super(oldrec_, newrec_, indrec_, attr_);

END Check_Common___;

@Override
PROCEDURE Check_Update___ (
   oldrec_ IN     customer_order_tab%ROWTYPE,
   newrec_ IN OUT customer_order_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
   CURSOR get_state IS 
		SELECT objstate
		FROM   customer_order
		WHERE  order_no = newrec_.order_no;
   state_		       CUSTOMER_ORDER.objstate%TYPE;
   company_               VARCHAR2(20);
BEGIN
   super(oldrec_, newrec_, indrec_, attr_);
   --(+)230901 TecFarahS M-2.8.2-BR001(START)
   IF Validate_SYS.Is_Changed(oldrec_.customer_no_pay, newrec_.customer_no_pay) AND newrec_.rowstate <> 'Planned' THEN
      Error_SYS.Record_General(lu_name_,'INVCUSTERR2: Invoice Customer Changes are allowed only if CO is in Planned status');
   END IF;
   --(+)230901 TecFarahS M-2.8.2-BR001(FINISH)   

   --(+) 231121 TecVaruSa MERGE-ENVECON-1 (START)
   OPEN  get_state;
   FETCH get_state INTO state_;
   CLOSE get_state;
	IF UPPER(state_) = 'INVOICED' THEN	
      IF newrec_.district_code != oldrec_.district_code THEN
         Error_SYS.RECORD_GENERAL(lu_name_,'UPDDISTRICTCODE: Cannot Update District Code when state is Invoiced/Closed');
      END IF;
      IF newrec_.market_code != oldrec_.market_code THEN
         Error_SYS.RECORD_GENERAL(lu_name_,'UPDMARKETCODE: Cannot Update Market Code when state is Invoiced/Closed');
      END IF;
      IF newrec_.region_code != oldrec_.region_code THEN
         Error_SYS.RECORD_GENERAL(lu_name_,'UPDREGIONCODE: Cannot Update Region Code when state is Invoiced/Closed');
      END IF;
	END IF;	
   Cust_Ord_Amendment_Detail_API.Check_AmendLine_Exist(newrec_.order_no,NULL,NULL,NULL,'Payment Term');
   IF newrec_.freight_flag IS NULL THEN
      newrec_.freight_flag := 'N';
   END IF;
   company_ := Site_API.Get_Company(newrec_.contract);
   IF (Customer_Info_Address_API.get_state(newrec_.customer_no,newrec_.ship_addr_no)) != (Site_Address_API.get_state_code(newrec_.contract)) THEN
      IF (Comp_Form_Type_API.get_inter_state(company_,newrec_.form_type) = 'FALSE') THEN
         Error_SYS.Record_Exist(lu_name_, 'NOINTERSTATE:  Not a Valid Inter State Form Type');
      END IF;
   ELSE
      IF (Comp_Form_Type_API.get_inter_state(company_,newrec_.form_type) = 'TRUE') THEN
         Error_SYS.Record_Exist(lu_name_, 'NOINTERSTATE:  Not a Valid Intra State Form Type');
      END IF;
   END IF;

   IF newrec_.sales_contract_no IS NOT NULL THEN
      FOR i IN (SELECT Sales_Unit_Meas 
      FROM customer_order_line_tab
      WHERE order_no = newrec_.order_no)
      LOOP
         $IF Component_Conmgt_SYS.INSTALLED $THEN
            IF i.Sales_Unit_Meas!= 
               Contract_Item_API.Get_Unit_Of_Measure( newrec_.sales_contract_no,
                                                      newrec_.contract_rev_seq,
                                                      newrec_.contract_line_no,
                                                      newrec_.contract_item_no) THEN
               Error_SYS.Record_General(lu_name_ ,'CHECKUOM: The Sales Contract Line Item UOM And The Order Line UOM Should To Be Same.');
            END IF;  
         $ELSE
            NULL;
         $END
      END LOOP;          
   END IF;
   --(+) 231121 TecVaruSa MERGE-ENVECON-1 (FINISH)
END Check_Update___;

@Override
PROCEDURE Update___ (
   objid_      IN     VARCHAR2,
   oldrec_     IN     CUSTOMER_ORDER_TAB%ROWTYPE,
   newrec_     IN OUT CUSTOMER_ORDER_TAB%ROWTYPE,
   attr_       IN OUT VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   by_keys_    IN     BOOLEAN DEFAULT FALSE )
IS
   chk_                VARCHAR2(5) := 'FALSE';
     --  170227  KINEIN   <C_G1768297-1 GST Development> start
   CURSOR get_lines_count (order_no_ IN VARCHAR2) IS
      --SELECT count(*)
      SELECT *
      FROM   CUSTOMER_ORDER_LINE_TAB
      WHERE  order_no = order_no_
      AND    rowstate <> 'Cancelled';

      inv_purch_flag_ VARCHAR2(5) := 'FALSE';
      Strcode_	      CUST_TAXES_HEAD_TAB.STRCODE%TYPE;
  --  170227  KINEIN   <C_G1768297-1 GST Development> End
   country_changed_  VARCHAR2(100);
BEGIN
   --(+)230330 TecGovinT M-CSS-082 (START)
   country_changed_ := NVL(Client_SYS.Get_Item_Value('CHANGED_COUNTRY_CODE', attr_),'FALSE');
   IF newrec_.rowstate IN ('Delivered','PartiallyDelivered','Picked','Released','Reserved') 
      AND (Site_Discom_Info_API.Get_C_Enable_Co_Chg_Ord(newrec_.contract) = 'True' 
      OR newrec_.c_co_change_order = 'TRUE')
      AND (NVL(Client_SYS.Get_Item_Value('COCO_UPDATE', attr_),'FALSE') = 'FALSE'
      AND (newrec_.c_co_change_order = 'TRUE'
      OR newrec_.rowstate IN ('Delivered','PartiallyDelivered','Picked','Released','Reserved')))
      AND  country_changed_= 'FALSE'
      THEN
      IF oldrec_.wanted_delivery_date            <> newrec_.wanted_delivery_date OR
         NVL(oldrec_.ship_via_code,'XYZ')        <> NVL(newrec_.ship_via_code,'XYZ') OR 
         oldrec_.delivery_terms                  <> newrec_.delivery_terms OR 
         oldrec_.ship_addr_no                    <> newrec_.ship_addr_no OR 
         NVL(oldrec_.country_code,'XYZ')         <> NVL(newrec_.country_code,'XYZ') OR 
         oldrec_.addr_flag                       <> newrec_.addr_flag  OR 
         NVL(oldrec_.del_terms_location,'XYZ')   <> NVL(newrec_.del_terms_location,'XYZ') OR 
         NVL(oldrec_.bill_addr_no,'XYZ')         <> NVL(newrec_.bill_addr_no,'XYZ') THEN 
         Error_SYS.Record_General(lu_name_,'CUPDNOTALLOWED: Values must be updated through a CO Change Order for this Customer Order');
      END IF;
   END IF;
   --(+)230330 TecGovinT M-CSS-082 (FINISH)
   
   --(+) 231121 TecVaruSa MERGE-ENVECON-1 (START)
   IF oldrec_.freight_flag <> newrec_.freight_flag THEN
      IF (Trans_Ship_Location_API.check_rec_exist(newrec_.ordeR_no)) THEN
         IF (newrec_.freight_flag = 'F' ) THEN
            chk_ :=	Trans_Ship_Location_API.check_fob_flag(newrec_.order_no);
            IF chk_ = 'FALSE' THEN
               Error_SYS.Record_General(lu_name_, 'FOBFLGERROR: No FOB destination Defined in Transhipment Details');
            END IF;
         ELSIF (newrec_.freight_flag = 'E') THEN
            chk_ :=	Trans_Ship_Location_API.check_fob_flag(newrec_.order_no);
            IF chk_ = 'TRUE' THEN
               Error_SYS.Record_General(lu_name_, 'FOBFLGERROR1: FOB destination Defined in Transhipment Details');
            END IF;
         END IF;
      END IF;
      IF (newrec_.freight_flag = 'N') THEN
         IF Trans_Ship_Location_API.check_rec_exist(newrec_.order_no) THEN
            Error_SYS.Record_General(lu_name_, 'REC_EXIST_ERROR: Details Exist In Transhipment Details Cannot be Moved To NotApplicable');
         END IF;	
      END IF;
   END IF;
   IF   newrec_.site_division <> NVL(oldrec_.site_division,SITE_API.Get_Site_Division(newrec_.contract))  THEN

     IF newrec_.site_division <> SITE_API.Get_Site_Division(newrec_.contract) THEN
	    ERROR_SYS.Record_General(lu_name_ , 'ERRDIV: Not a Valid division ,Division Should Be Defined At Site');
	 END IF;

   END IF;
   super(objid_, oldrec_, newrec_, attr_, objversion_, by_keys_);
   IF (newrec_.form_type <> oldrec_.form_type) OR (oldrec_.form_type IS NULL AND newrec_.form_type IS NOT NULL) THEN
      Update_Sales_Tax(newrec_);
   END IF;
     --  170227  KINEIN   <C_G1768297-1 GST Development> start
    inv_purch_flag_ :=  NVL(COMPANY_TAX_INFO_API.Get_Inv_Purch_Flag(Site_API.Get_Company(newrec_.contract)),'FALSE');
   IF inv_purch_flag_ = 'TRUE' THEN
    IF (newrec_.BILL_ADDR_NO <> oldrec_.BILL_ADDR_NO) THEN
       IF customer_order_api.Get_State(newrec_.order_no) = 'Planned' THEN
         FOR linerec_ IN get_lines_count(newrec_.order_no)
           LOOP
            Strcode_ := CUST_ORDER_TAXES_HEAD_API.Get_Strcode(linerec_.order_no, linerec_.line_no, linerec_.rel_no, linerec_.line_item_no);
  		   	  IF Strcode_ IS NOT NULL THEN
 				   	     CUST_ORDER_LINE_TAXES_API.Modify_HSN_Tax_Rate(linerec_.order_no, linerec_.line_no, linerec_.rel_no, linerec_.line_item_no,linerec_.c_hsn_sac_code);
            END IF;
           END LOOP;
       ELSE
           ERROR_SYS.Record_General('CustomerOrder','ERRGSTLN: Document Address cannot be changed as Customer Order Not in Planned State ');
       END IF;
    END IF;
   END IF;
    --  170227  KINEIN   <C_G1768297-1 GST Development> End
    --(+) 231121 TecVaruSa MERGE-ENVECON-1 (FINISH)
END Update___;

--(+) 231121 TecVaruSa MERGE-ENVECON-1 (START)
@Override
PROCEDURE Get_Order_Defaults___ (
   attr_           IN OUT VARCHAR2,
   all_attributes_ IN     VARCHAR2 DEFAULT 'TRUE' )
IS
   customer_no_                customer_order_tab.customer_no%TYPE;
BEGIN
   customer_no_    := Client_SYS.Get_Item_Value('CUSTOMER_NO', attr_);
   super(attr_, all_attributes_);
   Client_SYS.Set_Item_Value('EXPORT_TYPE',nvl(cust_ord_customer_api.get_export_type(customer_no_),'Domestic'),attr_);
END Get_Order_Defaults___;
--(+) 231121 TecVaruSaTecVaruSa MERGE-ENVECON-1 (FINISH)

@Override
PROCEDURE Finite_State_Set___ (
   rec_   IN OUT customer_order_tab%ROWTYPE,
   state_ IN     VARCHAR2 )
IS
   --(+) 2306113 TecGovinT M-CSS-6.8.9-001 (START)
   info_          VARCHAR2(1000);
   attr_          VARCHAR2(1000);
   objid_         VARCHAR2(50);
   objversion_    VARCHAR2(50);
   line_count_    NUMBER;
   
   
   $IF Component_Crm_SYS.INSTALLED $THEN
   CURSOR get_bo_line_count IS 
      SELECT count(*)
      FROM business_opportunity_line_tab
      WHERE con_object_ref1 = rec_.order_no
      AND rowstate          != 'Won';
   
   CURSOR get_bo_details IS
      SELECT ROWID,
             TO_CHAR(rowversion,'YYYYMMDDHH24MISS')
      FROM business_opportunity_tab
      WHERE opportunity_no = rec_.business_opportunity_no;
   $END
   
   $IF Component_Cmod_SYS.INSTALLED $THEN
   CURSOR get_cust_peg_requests IS 
      SELECT DISTINCT cmp.req_no,
             cmp.req_line_no
      FROM c_manual_pegging_line_tab cmp
      WHERE cmp.req_no IN (SELECT req_no 
                           FROM c_manual_pegging_line_tab 
                           WHERE co_order_no = rec_.order_no)
      AND cmp.shipment_id IS NOT NULL;
   $END
   
   CURSOR get_closed_req_count (req_no_   NUMBER,line_no_   VARCHAR2 ) IS 
      SELECT COUNT(*)
      FROM c_manual_pegging_line_tab cmp,
           customer_order_tab co
      WHERE co.order_no=cmp.co_order_no
      AND cmp.req_no       = req_no_
      AND cmp.req_line_no  = line_no_
      AND co.rowstate      <> 'Invoiced';      
   --(+) 2306113 TecGovinT M-CSS-6.8.9-001 (FINISH)
   --(+)230911 TecFarahS M-11.3.5-BR001(START)
   $IF Component_Mromfg_SYS.INSTALLED $THEN
      CURSOR get_int_hdr_id IS
         SELECT DISTINCT ios.interim_header_id 
         FROM     customer_order_tab            co,
                  customer_order_line_tab       col,
                  interim_order_supply_ext_uiv  ios
         WHERE co.order_no = col.order_no
         AND   col.demand_order_ref1 = ios.visit_id
         AND   co.order_no = rec_.order_no
         AND   ios.interim_order_state <>'Closed';
     $END
   --(+)230911 TecFarahS M-11.3.5-BR001(FINISH)
BEGIN

   super(rec_, state_);
   --(+) 2306113 TecGovinT M-CSS-6.8.9-001 (START)
   IF state_ = 'Invoiced' THEN
      $IF Component_Crm_SYS.INSTALLED $THEN
      OPEN  get_bo_line_count;
      FETCH get_bo_line_count INTO line_count_;
      CLOSE get_bo_line_count;
      
      IF line_count_ = 0 THEN 
         OPEN  get_bo_details;
         FETCH get_bo_details INTO objid_,objversion_;
         CLOSE get_bo_details;
         
         IF objid_ IS NOT NULL THEN 
            Business_Opportunity_API.Set_Closed__(info_,objid_,objversion_,attr_,'DO');
         END IF;
      END IF;
      
      $IF Component_Cmod_SYS.INSTALLED $THEN
      FOR rec_ IN get_cust_peg_requests
         LOOP
            OPEN  get_closed_req_count(rec_.req_no,rec_.req_line_no);
            FETCH get_closed_req_count INTO line_count_;
            CLOSE get_closed_req_count;
            
            IF line_count_ = 0 THEN 
               C_Customer_Request_API.Validate_And_Close(rec_.req_no,rec_.req_line_no);
            END IF;
         END LOOP;
      $END
      $ELSE
      NULL;
      $END
   END IF;
   --(+) 2306113 TecGovinT M-CSS-6.8.9-001 (FINISH)
   --(+) 230823 TecKarunkarS M-CSS-134_141_142 (START))
   IF state_ = 'Delivered' THEN
     $IF Component_Cmod_SYS.INSTALLED $THEN
     C_Param_Warranty_Dtls_API.Add_Param_Warranty(rec_.order_no,'CO');
     C_Lot_Batch_Warranty_Dates_API.Add_Lot_Batch_Warranty(rec_.order_no,'CO');
     $ELSE
      NULL;
     $END
     END IF;
     
   --(+) 230823 TecKarunkarS M-CSS-134_141_142 (FINISH))
   --(+)230911 TecFarahS M-11.3.5-BR001(START)
   IF state_ = 'Invoiced' THEN
      $IF Component_Mromfg_SYS.INSTALLED $THEN  
      FOR getrec_ IN get_int_hdr_id
      LOOP
         Interim_Mro_Manager_API.Close_Shop_Orders(getrec_ .interim_header_id);
      END LOOP;
      $ELSE
      NULL;
      $END
   END IF;
   --(+)230911 TecFarahS M-11.3.5-BR001(FINISH)   
END Finite_State_Set___;

--(+) 231121 TecVaruSa MERGE-ENVECON-1 (START)
@Override
PROCEDURE Set_Released__ (
   info_       OUT    VARCHAR2,
   objid_      IN     VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   attr_       IN OUT VARCHAR2,
   action_     IN     VARCHAR2 )
IS
   rec_ customer_order_tab%ROWTYPE;
   
   --(+) 231219 SB00812057 FS ID : M-11.3.5-BR001 (START)
   m_budget_ref_ customer_order_tab.c_customer_budget_ref%TYPE;
   
   CURSOR get_budget_code_ IS
      SELECT c_customer_budget_ref 
      FROM   customer_order_tab
      WHERE  rowid = objid_;
   --(+) 231219 SB00812057 FS ID : M-11.3.5-BR001 (FINISH),
   
   CURSOR get_ct3 IS
      SELECT count(*) ncount FROM ORDER_LINE_EXP_INFO 
      WHERE order_no = rec_.order_no;
   CURSOR get_bond IS
      SELECT count(*) ncount FROM ORDER_bond_INFO t
      WHERE order_no = rec_.order_no;
   ncount_ NUMBER := 0;
   
   CURSOR get_excise(order_no_ VARCHAR2) IS
      SELECT    count(*) ncount
      FROM CUST_ORDER_LINE_TAXES
      WHERE order_no = order_no_
      AND nvl(ct3_bond_clearance,'FALSE')='TRUE'
      AND tax_type = 'Excise'
      AND nvl(rate,0) != 0  ;
	rno_  	VARCHAR2(20);
	stat_  	VARCHAR2(253);
	bnk_     VARCHAR2(50);
	portl_	VARCHAR2(20);
	portd_	VARCHAR2(20);
	lcamt_	NUMBER;
	amt_     NUMBER;
      
BEGIN
   --(+) 231219 SB00812057 FS ID : M-11.3.5-BR001 (START)
   OPEN get_budget_code_;
   FETCH get_budget_code_ INTO m_budget_ref_;
   CLOSE get_budget_code_;
   
   IF m_budget_ref_ IS NULL THEN
      Error_SYS.Record_General(Lu_Name_,'CHKBUDERR: Customer Budget Ref mandatory');
   END IF;
   --(+) 231219 SB00812057 FS ID : M-11.3.5-BR001 (FINISH)
   
   IF (action_ = 'DO') THEN
      rec_ := Lock_By_Id___(objid_, objversion_);
      IF nvl(Company_Tax_Info_API.Get_Inv_Purch_Flag(Site_API.Get_Company(rec_.contract)),'FALSE') != 'FALSE'
         AND Company_Tax_Info_API.Get_GST_EFFECTIVE_FROM(Site_API.Get_Company(rec_.contract)) IS NOT NULL THEN
         
         IF  (Company_Tax_Info_API.Get_GST_EFFECTIVE_FROM(Site_API.Get_Company(rec_.contract))>  rec_.date_entered) THEN
            IF   Customer_Order_API.get_form_type(rec_.order_no) IS NULL THEN
               Error_SYS.Record_General(Lu_Name_,'Please Enter Form Type before Releasing');     
            END IF;
         ELSE
            --  170314  KINEIN   <C_G1768297-1 GST Development> start         
				Cust_Order_Line_Taxes_API.Check_HSN_Tax_Rate(rec_.order_no);
            --  170314  KINEIN   <C_G1768297-1 GST Development> End
         END IF;         
      END IF;
      IF (Customer_Order_API.get_export_type(rec_.order_no)  =  'Deemed Export' OR Customer_Order_API.get_export_type(rec_.order_no) = 'Merchant Export' ) AND nvl(Order_Exp_Info_API.get_bypass_ct(rec_.order_no),'FALSE') = 'FALSE' THEN
         ncount_ := 0;
         FOR i IN get_ct3
         LOOP
            ncount_ := nvl(i.ncount,0);
         END LOOP;
         IF ncount_ = 0 THEN
            Error_SYS.Record_General(Lu_Name_,'CTINFOREQ: Please enter CT Information against order');
         END IF;
      END IF;
      IF (Customer_Order_API.get_export_type(rec_.order_no)  =  'Deemed Export' OR Customer_Order_API.get_export_type(rec_.order_no) = 'Merchant Export' )
         AND nvl(Order_Exp_Info_API.get_bypass_ct(rec_.order_no),'FALSE') = 'TRUE' THEN
         ncount_ := 0;
         FOR i IN get_excise(rec_.order_no)
         LOOP
            ncount_ := nvl(i.ncount,0);
         END LOOP;
         IF ncount_ != 0 THEN
            Error_SYS.Record_General(Lu_Name_,'CTFLAGBYPASS: CT flag is Bypassed. Please change the Tax Structure');
         END IF;
      END IF;
      IF Customer_Order_API.get_export_type(rec_.order_no)  =  'Direct Export' 
         AND nvl( Order_Bond_Info_API.get_bypass_bond(rec_.order_no),'FALSE') = 'FALSE'  THEN
         ncount_ := 0;
         FOR i IN get_bond
         LOOP
            ncount_ := nvl(i.ncount,0);
         END LOOP;
         IF ncount_ = 0 THEN
            Error_SYS.Record_General(Lu_Name_,'BONDINFOREQ: Please enter Bond information against order');
         END IF;
      END IF;
      IF Customer_Order_API.get_export_type(rec_.order_no)  =  'Direct Export' 
         AND nvl( Order_Bond_Info_API.get_bypass_bond(rec_.order_no),'FALSE') = 'TRUE'  THEN
         ncount_ := 0;
         FOR i IN get_excise(rec_.order_no)
         LOOP
            ncount_ := nvl(i.ncount,0);
         END LOOP;
         IF ncount_ != 0 THEN
            Error_SYS.Record_General(Lu_Name_,'BONDFLAGBYPASS: Bond flag is Bypassed. Please change the Tax Structure');
         END IF;
      END IF;
   END IF;
   super(info_, objid_, objversion_, attr_, action_);
   IF (action_ = 'DO') THEN
      IF rec_.payment_method = 'LC' THEN
         BEGIN
            SELECT REFERENCE_NUMBER INTO rno_ FROM LC_ORDER_DETAIL
            WHERE ORDER_NO LIKE rec_.order_no
            AND COMPANY LIKE SITE_API.GET_COMPANY(rec_.CONTRACT);
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               Error_SYS.Record_General(lu_name_, 'LCNTOPEN: LC is not opened against this Customer Order');		       
         END;
         SELECT STATE INTO stat_ FROM EXPORT_LC_HEADER
         WHERE REFERENCE_NUMBER LIKE rno_
         AND COMPANY LIKE SITE_API.GET_COMPANY(rec_.CONTRACT);
         IF stat_= 'Approved' THEN
            amt_   := Lc_Order_Detail_API.Get_Total_Order_Amount(Site_API.GET_COMPANY(rec_.CONTRACT),rno_);
            lcamt_ := Export_Lc_Header_API.GET_LC_AMOUNT(rno_, Site_API.GET_COMPANY(rec_.CONTRACT));
            IF lcamt_ < amt_ THEN
               Error_SYS.Record_General(lu_name_, 'LCAMT: LC amount is not sufficient for this Customer Order');		        
            END IF;
         ELSE
            Error_SYS.Record_General(lu_name_, 'LCMOD: LC modification is pending');		        
         END IF;
         bnk_:=Export_Lc_Header_API.GET_ADVISING_BANK(rno_, Site_API.GET_COMPANY(rec_.CONTRACT));
         IF bnk_ != rec_.DISCOUNTING_BANK THEN
            Error_SYS.Record_General(lu_name_, 'DISCBNK: Discounting bank should be same as Advisory Bank in LC');		        
         END IF;
         portl_ := Export_Lc_Header_API.GET_PORT_OF_LOADING(rno_, Site_API.GET_COMPANY(rec_.CONTRACT));
         IF portl_ != rec_.PORT_OF_LOADING THEN
            Error_SYS.Record_General(lu_name_, 'POL: Port of Loading should be that of LC');		        
         END IF;
         portd_ := Export_Lc_Header_API.GET_PORT_OF_DISCHARGE(rno_, Site_API.GET_COMPANY(rec_.CONTRACT));
         IF portd_ != rec_.PORT_OF_DISCHARGE THEN
            Error_SYS.Record_General(lu_name_, 'POD: Port of Discharge should be same as that of LC');		        
         END IF;
      END IF;	
   END IF;
END Set_Released__;
--(+) 231121 TecVaruSa MERGE-ENVECON-1 (FINISHs)
-------------------- LU SPECIFIC PRIVATE METHODS ----------------------------
--(+) 231121 TecVaruSa MERGE-ENVECON-1 (START)
@Overtake Core
FUNCTION Get_Total_Sale_Price__ (
   order_no_ IN VARCHAR2,
   hsn_sac_code_ IN VARCHAR2 DEFAULT NULL ) RETURN NUMBER
IS
BEGIN
   $SEARCH
   total_sale_price_ := Get_Total_Sale_Price___(order_no_, FALSE);
   $REPLACE
   IF Nvl(GAdvOnHsn_,'FALSE') = 'TRUE' THEN
      total_sale_price_ := Get_Total_Sale_Price___(order_no_, FALSE, hsn_sac_code_);
   ELSE
      total_sale_price_ := Get_Total_Sale_Price___(order_no_, FALSE);
   END IF;      
   $END
END Get_Total_Sale_Price__;
--(+) 231121 TecVaruSa MERGE-ENVECON-1 (FINISH)

-------------------- LU SPECIFIC PROTECTED METHODS --------------------------


-------------------- LU SPECIFIC PUBLIC METHODS -----------------------------
@Override
PROCEDURE Set_Cancelled (
   order_no_ IN VARCHAR2 )
IS
   --(+)230305 TecGovinT M-CSS-082 (START)
   info_          VARCHAR2(1000);
   attr_          VARCHAR2(1000);
   objid_         VARCHAR2(50);
   objversion_    VARCHAR2(50);
   
   CURSOR check_coco_exist IS 
      SELECT ROWID,
             TO_CHAR(rowversion,'YYYYMMDDHH24MISS')
      FROM c_cust_chg_ord_tab
      WHERE order_no = order_no_
      AND rowstate   NOT IN ('Cancelled','COUpdated');
   --(+)230305 TecGovinT M-CSS-082 (FINISH)      
BEGIN
   super(order_no_);
   --(+)230305 TecGovinT M-CSS-082 (START)
   OPEN  check_coco_exist;
   FETCH check_coco_exist INTO objid_,objversion_;
   CLOSE check_coco_exist;
   
   IF objid_ IS NOT NULL THEN
      C_Cust_Chg_Ord_API.Set_Cancelled__(info_,objid_,objversion_,attr_,'DO');
   END IF;
   --(+)230305 TecGovinT M-CSS-082 (FINISH)
END Set_Cancelled;

   
@Override
PROCEDURE Set_Released (
   order_no_ IN VARCHAR2 )
IS
   --(+) 221112 TecKarunkarS  M-CSS-013A (START)
   netbase_           NUMBER;
   netcur_            NUMBER;
   grosscur_          NUMBER;
   info_              VARCHAR2(1000);
   objid_             VARCHAR2(50);
   objversion_        VARCHAR2(100);
   attr_              VARCHAR2(1000);
   
   CURSOR get_totals IS
   SELECT order_line_total_base,
          order_line_total_curr,
          order_line_gross_total_curr
   FROM   Customer_Order_API.Calculate_Totals(order_no_);
   --(+) 221112 TecKarunkarS  M-CSS-013A (FINISH)
BEGIN
   --(+) 221112 TecKarunkarS  M-CSS-013A (START)
   super(order_no_);
   
   OPEN  get_totals;
   FETCH get_totals INTO netbase_,netcur_,grosscur_;
   CLOSE get_totals;
   
   Get_Id_Version_By_Keys(objid_, objversion_,  order_no_);
   
   Client_SYS.Add_To_Attr('C_NET_AMOUNT_BASE_AT_REL', netbase_, attr_);
   Client_SYS.Add_To_Attr('C_NET_AMOUNT_AT_REL',      netcur_,  attr_);
   Client_SYS.Add_To_Attr('C_GROSS_AMOUNT_AT_REL',    grosscur_,attr_);
   
   Modify__(info_, objid_, objversion_, attr_, 'DO');
   --(+) 221112 TecKarunkarS  M-CSS-013A (START)
   
   --(+) 230823 TecKarunkarS M-CSS-134_141_142 (START))
   Customer_Order_Line_API.C_Cust_Warr_Temp_Mapping (order_no_);
   --(+) 230823 TecKarunkarS M-CSS-134_141_142 (FINISH)
END Set_Released;

--(+) 231121 TecVaruSa MERGE-ENVECON-1 (START)
@Overtake Core
FUNCTION Get_Gross_Amount (
   order_no_ IN VARCHAR2,
   hsn_sac_code_ IN VARCHAR2 DEFAULT NULL ) RETURN NUMBER
IS
BEGIN
$SEARCH
      total_net_amount_   := Get_Total_Base_Price(order_no_);
      total_tax_amount_   := Get_Total_Tax_Amount(order_no_) ;
      total_gross_amount_ := total_net_amount_ + total_tax_amount_;
$REPLACE
   IF NVL(GAdvOnHsn_,'FALSE') = 'TRUE' THEN
      total_net_amount_           := Get_Total_Base_Price(order_no_,hsn_sac_code_);
      total_tax_amount_           := Get_Total_Tax_Amount(order_no_,hsn_sac_code_) ;
      total_gross_amount_         := total_net_amount_ + total_tax_amount_;
   ELSE   
      total_net_amount_           := Get_Total_Base_Price(order_no_);
      total_tax_amount_           := Get_Total_Tax_Amount(order_no_) ;
      total_gross_amount_         := total_net_amount_ + total_tax_amount_;
   END IF;   
$END
END Get_Gross_Amount;

@Overtake Core
FUNCTION Get_Ord_Gross_Amount (
   order_no_ IN VARCHAR2,
   hsn_sac_code_ IN VARCHAR2 DEFAULT NULL ) RETURN NUMBER
IS
BEGIN
$SEARCH
   total_gross_amount_ := Get_Total_Sale_Price___(order_no_ , TRUE) + Get_Ord_Total_Tax_Amount___(order_no_,TRUE);
$REPLACE
   IF Nvl(GAdvOnHsn_,'FALSE') = 'TRUE' THEN
      total_gross_amount_ := Get_Total_Sale_Price___(order_no_ , TRUE,hsn_sac_code_) + Get_Ord_Total_Tax_Amount___(order_no_,TRUE,hsn_sac_code_);
   ELSE
      total_gross_amount_ := Get_Total_Sale_Price___(order_no_ , TRUE) + Get_Ord_Total_Tax_Amount___(order_no_,TRUE);
   END IF;   
$END
END Get_Ord_Gross_Amount;

@Overtake Core
FUNCTION Get_Ord_Total_Tax_Amount (
   order_no_ IN VARCHAR2,
   hsn_sac_code_ IN VARCHAR2 DEFAULT NULL ) RETURN NUMBER
IS
BEGIN
$SEARCH
   total_tax_amount_:= Get_Ord_Total_Tax_Amount___(order_no_,FALSE);
$REPLACE
   IF NVL(GAdvOnHsn_,'FALSE') = 'TRUE' THEN
       total_tax_amount_:= Get_Ord_Total_Tax_Amount___(order_no_,FALSE,hsn_sac_code_);
   ELSE   
       total_tax_amount_:= Get_Ord_Total_Tax_Amount___(order_no_,FALSE);
   END IF; 
$END
END Get_Ord_Total_Tax_Amount;

@Overtake Core
PROCEDURE Copy_Customer_Order_Header___ (
   to_order_no_          IN OUT VARCHAR2,
   from_order_no_        IN     VARCHAR2, 
   customer_no_          IN     VARCHAR2,
   order_id_             IN     VARCHAR2,
   currency_code_        IN     VARCHAR2,
   contract_             IN     VARCHAR2,
   wanted_delivery_date_ IN     DATE,
   copy_order_adresses_  IN     VARCHAR2,
   copy_misc_order_info_ IN     VARCHAR2,      
   copy_delivery_info_   IN     VARCHAR2,
   copy_document_info_   IN     VARCHAR2, 
   copy_tax_detail_      IN     VARCHAR2,
   copy_pricing_         IN     VARCHAR2,
   copy_document_texts_  IN     VARCHAR2,
   copy_notes_           IN     VARCHAR2,   
   copy_pre_accounting_  IN     VARCHAR2,
   copy_charges_         IN     VARCHAR2)
IS
BEGIN
   $SEARCH
   Client_SYS.Add_To_Attr('WANTED_DELIVERY_DATE', NVL(wanted_delivery_date_, copy_order_rec_.wanted_delivery_date) , attr_);
   $APPEND
   Client_SYS.Add_To_Attr('FREIGHT_FLAG', NVL(Freight_Flag_API.Decode(copy_order_rec_.freight_flag), 'NotApplicable') , attr_);
   $END
END Copy_Customer_Order_Header___;
--(+) 231121 TecVaruSa MERGE-ENVECON-1 (FINISh)




-------------------- LU EXT NEW METHODS -------------------------------------
--(+) 231121 TecVaruSa MERGE-ENVECON-1 (START)
FUNCTION Get_Ord_Total_Tax_Amount___ (
   order_no_     IN VARCHAR2,
   exclude_item_ IN BOOLEAN,
   hsn_sac_code_ IN VARCHAR2) RETURN NUMBER
IS
   total_tax_amount_  NUMBER := 0;
   next_line_tax_     NUMBER := 0;
   ordrec_            CUSTOMER_ORDER_TAB%ROWTYPE;
   company_           VARCHAR2(20);
   rounding_          NUMBER;

   CURSOR get_lines IS
      SELECT line_no, rel_no, line_item_no, currency_rate, price_conv_factor
      FROM CUSTOMER_ORDER_LINE_TAB
      WHERE order_no = order_no_
      AND   line_item_no <= 0
      AND   Nvl(c_hsn_sac_code,'%') LIKE NVL(hsn_sac_code_,'%')
      AND   rowstate != 'Cancelled';   
   CURSOR get_lines_excl_item IS
      SELECT line_no, rel_no, line_item_no, currency_rate, price_conv_factor
      FROM CUSTOMER_ORDER_LINE_TAB
      WHERE order_no = order_no_
      AND   line_item_no <= 0
      AND   NVL(c_hsn_sac_code,'%') LIKE NVL(hsn_sac_code_,'%')
      AND   rowstate != 'Cancelled'
      AND   charged_item != 'ITEM NOT CHARGED'
      AND   exchange_item !='EXCHANGED ITEM';
BEGIN
   ordrec_   := Get_Object_By_Keys___(order_no_);
   company_  := Site_API.Get_Company(ordrec_.contract);
   rounding_ := Currency_Code_API.Get_Currency_Rounding(company_,ordrec_.currency_code);
   IF (exclude_item_) THEN
      FOR next_line_ IN get_lines_excl_item LOOP
         next_line_tax_ := Customer_Order_Line_API.Get_Total_Tax_Amount_Curr(order_no_,
                                                                             next_line_.line_no,
                                                                             next_line_.rel_no,
                                                                             next_line_.line_item_no);

         total_tax_amount_ := total_tax_amount_ + next_line_tax_;
      END LOOP;
   ELSE
      FOR next_line_ IN get_lines LOOP
         next_line_tax_ := Customer_Order_Line_API.Get_Total_Tax_Amount_Curr(order_no_,
                                                                             next_line_.line_no,
                                                                             next_line_.rel_no,
                                                                             next_line_.line_item_no);

         total_tax_amount_ := total_tax_amount_ + next_line_tax_;
      END LOOP;
   END IF;

   total_tax_amount_ := ROUND(total_tax_amount_, rounding_);
   RETURN NVL(total_tax_amount_, 0);
END Get_Ord_Total_Tax_Amount___;

FUNCTION Get_Total_Sale_Price___ (
   order_no_     IN VARCHAR2,
   exclude_item_ IN BOOLEAN,
   hsn_sac_code_ IN VARCHAR2) RETURN NUMBER
IS
   total_sale_price_ NUMBER := 0;
   CURSOR get_lines IS
      SELECT line_no, rel_no, line_item_no 
      FROM   CUSTOMER_ORDER_LINE_TAB
      WHERE  order_no = order_no_
      AND    rowstate != 'Cancelled'
      AND    Nvl(c_hsn_sac_code,'%') LIKE NVL(hsn_sac_code_,'%')
      AND    line_item_no <= 0;
   CURSOR get_lines_excl_item IS
      SELECT line_no, rel_no, line_item_no 
      FROM   CUSTOMER_ORDER_LINE_TAB
      WHERE  order_no = order_no_
      AND    rowstate != 'Cancelled'
      AND    Nvl(c_hsn_sac_code,'%') LIKE NVL(hsn_sac_code_,'%')
      AND    line_item_no <= 0
      AND    charged_item != 'ITEM NOT CHARGED'
      AND    exchange_item != 'EXCHANGED ITEM';
BEGIN
   IF (exclude_item_ ) THEN
      FOR next_line_ IN get_lines_excl_item LOOP
         total_sale_price_ := total_sale_price_ + Customer_Order_Line_API.Get_Sale_Price_Total(order_no_, next_line_.line_no, next_line_.rel_no, next_line_.line_item_no) ;   
      END LOOP;
   ELSE
      FOR next_line_ IN get_lines LOOP
         total_sale_price_ := total_sale_price_ + Customer_Order_Line_API.Get_Sale_Price_Total(order_no_, next_line_.line_no, next_line_.rel_no, next_line_.line_item_no) ;
      END LOOP;
   END IF;
   RETURN NVL(total_sale_price_, 0);
END Get_Total_Sale_Price___;

FUNCTION Get_Total_Base_Price (
   order_no_ IN VARCHAR2,
   hsn_sac_code_ IN VARCHAR2) RETURN NUMBER
IS
      CURSOR get_lines IS
      SELECT line_no, rel_no, line_item_no
      FROM  CUSTOMER_ORDER_LINE_TAB
      WHERE rowstate != 'Cancelled'
      AND   line_item_no <= 0
      AND   order_no = order_no_
      AND   ((Nvl(GAdvOnHsn_,'FALSE') = 'FALSE') OR
             (NVL(GAdvOnHsn_,'FALSE') = 'TRUE' AND Nvl(c_hsn_sac_code,'%') LIKE NVL(hsn_sac_code_,'%')));
   total_base_price_ NUMBER:= 0;

BEGIN
   FOR rec_ IN get_lines LOOP
      total_base_price_ := total_base_price_ + Customer_Order_Line_API.Get_Base_Sale_Price_Total(order_no_, rec_.line_no, rec_.rel_no, rec_.line_item_no);
   END LOOP;
   RETURN NVL(total_base_price_, 0);
END Get_Total_Base_Price;

@Overtake Core
FUNCTION Get_Total_Tax_Amount (
   order_no_ IN VARCHAR2,
   hsn_sac_code_ IN VARCHAR2 DEFAULT NULL ) RETURN NUMBER
IS
      CURSOR get_lines IS
      SELECT line_no, rel_no, line_item_no
      FROM  CUSTOMER_ORDER_LINE_TAB
      WHERE rowstate != 'Cancelled'
      AND   line_item_no <= 0
      AND   order_no = order_no_
      AND   ((Nvl(GAdvOnHsn_,'FALSE') = 'FALSE') OR
             (NVL(GAdvOnHsn_,'FALSE') = 'TRUE' AND Nvl(c_hsn_sac_code,'%') LIKE NVL(hsn_sac_code_,'%')));
   total_tax_amount_ NUMBER:= 0;
BEGIN
   FOR next_line_ IN get_lines LOOP
      total_tax_amount_ := total_tax_amount_ +
                           Customer_Order_Line_API.Get_Total_Tax_Amount_Base(order_no_,
                                                                             next_line_.line_no,
                                                                             next_line_.rel_no,
                                                                             next_line_.line_item_no);
   END LOOP;
   RETURN total_tax_amount_;
END Get_Total_Tax_Amount;

FUNCTION Get_Order_Date1(
   order_no_ IN VARCHAR2) RETURN DATE 
IS
   ord_date_ DATE;
   CURSOR Get_Date(order_no_ VARCHAR2) IS
	select wanted_delivery_date
	from   customer_order_tab
	where  order_no=order_no_;						   
BEGIN 
	OPEN Get_Date(order_no_);
	FETCH Get_Date INTO ord_date_;	     
	CLOSE Get_Date;
	RETURN ord_date_;
END Get_Order_Date1;

FUNCTION Get_req_Prefix__ (
   order_no_ IN VARCHAR2 ) RETURN VARCHAR2
IS
   temp_ CUSTOMER_ORDER_TAB.req_Prefix%TYPE;
   CURSOR get_attr IS
      SELECT req_Prefix
      FROM   CUSTOMER_ORDER_TAB
      WHERE  order_no = order_no_;
BEGIN
   OPEN get_attr;
   FETCH get_attr INTO temp_;
   CLOSE get_attr;
   RETURN temp_;
END Get_req_Prefix__;


PROCEDURE Update_CO_Authorize(
   status_	         IN OUT VARCHAR2,
   order_no_			IN  VARCHAR2)
IS
   objid1_           VARCHAR2(2000);
   objversion1_   	VARCHAR2(2000);
   info_             VARCHAR2(2000);
   attr1_            VARCHAR2(2000);
   CURSOR get_version IS
      SELECT ROWID, ltrim(lpad(to_char(rowversion,'YYYYMMDDHH24MISS'),2000))
      FROM  CUSTOMER_ORDER_TAB
      WHERE order_no = order_no_;
BEGIN
   status_ := 'FALSE';
   OPEN  get_version;
   FETCH get_version INTO  objid1_,objversion1_;
   CLOSE get_version;
   Client_SYS.Clear_Attr(attr1_);
   Client_SYS.Add_To_Attr('AUTHORIZE_STATUS','Authorized',attr1_);
   Customer_Order_API.MODIFY__(info_,objid1_,objversion1_, attr1_ ,'DO');
   status_ := 'TRUE';
END Update_CO_Authorize;


FUNCTION Get_Order_Taxes (
   order_no_	IN VARCHAR2) RETURN NUMBER
IS
   CURSOR c1 IS
      SELECT count(*)
      FROM cust_order_line_taxes_tab
      WHERE order_no = order_no_;
   counter_	NUMBER;
BEGIN
   OPEN c1;
   FETCH c1 INTO counter_;
   RETURN counter_;
   CLOSE c1;
END Get_Order_Taxes;


FUNCTION Get_Order_State (
   order_no_ IN VARCHAR2 ) RETURN VARCHAR2
IS
   CURSOR c1 IS
      SELECT rowstate
      FROM customer_order_tab
      WHERE order_no = order_no_;
   state_	   customer_order_tab.rowstate%TYPE;
BEGIN
   OPEN c1;
   FETCH c1 INTO state_;
   RETURN state_;
   CLOSE c1;
END Get_Order_State;

PROCEDURE Update_Sales_Tax(
   newrec_		IN CUSTOMER_ORDER_TAB%ROWTYPE)
IS
   attr_		VARCHAR2(32000);
   get_sales_tax_	 cust_order_line_taxes_tab%ROWTYPE;
   CURSOR Get_Sales_Tax  IS
      SELECT *
      FROM cust_order_line_taxes_tab
      WHERE order_no = newrec_.order_no
      AND tax_type = '2';
   form_type_taxes_	Comp_form_Type_line_Tab%ROWTYPE;
   CURSOR form_type_taxes (tax_code_ IN VARCHAR2) IS
      SELECT *
      FROM Comp_form_Type_line_Tab
      WHERE COMPANY 		= Site_Api.Get_Company(newrec_.contract)
      AND 	Form_Type   	= newrec_.Form_Type
      AND Form_Rate_Type  = newrec_.Form_Rate_Type
      AND Tax_Code 		= tax_code_
      AND Tax_Type		= 2	  	AND
      Rowstate		= 'Firm'		AND
      newrec_.date_entered BETWEEN FROM_DATE AND NVL(TILL_DATE,newrec_.date_entered);
BEGIN
   OPEN Get_Sales_Tax;
   LOOP
      FETCH Get_Sales_Tax INTO get_sales_tax_;
      EXIT WHEN Get_Sales_Tax%NOTFOUND;
      OPEN form_type_taxes(get_sales_tax_.tax_code);
      LOOP
         FETCH form_type_taxes INTO form_type_taxes_;
         EXIT WHEN form_type_taxes%NOTFOUND;
         Client_SYS.Clear_Attr(attr_);
         Client_SYS.Add_To_Attr('RATE', form_type_taxes_.rate, attr_);
         Client_SYS.Add_To_Attr('TAX_RATE_TYPE', Tax_Rate_Type_API.Decode('3'), attr_);
         Cust_Order_line_taxes_Api.Modify_Remove (
            attr_,
            get_sales_tax_.order_no,
            get_sales_tax_.line_no,
            get_sales_tax_.rel_no,
            get_sales_tax_.line_item_no,
            get_sales_tax_.strcode,
            get_sales_tax_.tax_no,
            'Modify');
      END LOOP;
      CLOSE form_type_taxes;
   END LOOP;
   CLOSE Get_Sales_Tax;
END  Update_Sales_Tax;

FUNCTION Invoiced_Status (
   order_no_ IN VARCHAR2 ) RETURN VARCHAR2
IS
BEGIN
   RETURN Get_State(order_no_);
END   Invoiced_Status;

FUNCTION Get_Total_Base_Price_Curr (
   order_no_ IN VARCHAR2,
   hsn_sac_code_ IN VARCHAR2 DEFAULT NULL ) RETURN NUMBER
IS
   ordrec_           CUSTOMER_ORDER_TAB%ROWTYPE;
   company_          VARCHAR2(20);
   rounding_         NUMBER;
   total_base_price_ NUMBER;

   CURSOR get_totals(rounding_ IN NUMBER ) IS
      SELECT SUM(ROUND((buy_qty_due * price_conv_factor * sale_unit_price), rounding_) -
                 ROUND((buy_qty_due * price_conv_factor * sale_unit_price) -
                       ((buy_qty_due * price_conv_factor * sale_unit_price) * ((1 - discount / 100) * (1 - (order_discount + additional_discount ) / 100))), rounding_))
      FROM  CUSTOMER_ORDER_LINE_TAB
      WHERE rowstate != 'Cancelled'
      AND   line_item_no <= 0
      AND   order_no = order_no_
      AND   ((Nvl(GAdvOnHsn_,'FALSE') = 'FALSE') OR
             (NVL(GAdvOnHsn_,'FALSE') = 'TRUE' AND Nvl(c_hsn_sac_code,'%') LIKE NVL(hsn_sac_code_,'%')));
BEGIN
   ordrec_ := Get_Object_By_Keys___(order_no_);
   company_ := Site_API.Get_Company(ordrec_.contract);
   rounding_ := Currency_Code_API.Get_Currency_Rounding(company_, Company_Finance_API.Get_Currency_Code(company_));
   OPEN get_totals(rounding_);
   FETCH get_totals INTO total_base_price_;
   IF (get_totals%NOTFOUND) THEN
      total_base_price_ := 0;
   END IF;
   CLOSE get_totals;

   RETURN NVL(total_base_price_, 0);
END Get_Total_Base_Price_Curr;

FUNCTION Get_Total_Charge(
   contract_ 				IN VARCHAR2,
   order_no_				IN VARCHAR2,
   currency_code_         IN VARCHAR2
   ) RETURN NUMBER
IS
   CURSOR Get_Order_Lines(contract_ IN VARCHAR2,order_no_ IN VARCHAR2) IS
      SELECT col.line_no,
             col.rel_no,
             col.line_item_no,
             col.sale_unit_price,
             col.base_sale_unit_price,
             col.buy_qty_due*col.price_conv_factor buy_qty_due
      FROM customer_order_line_tab col
      WHERE col.contract = contract_
      AND col.order_no = order_no_
      AND col.rowstate != 'Cancelled'
      AND EXISTS (SELECT 1 FROM cust_order_line_taxes_tab colt
         WHERE colt.order_no = order_no_
         AND colt.line_no = col.line_no
         AND colt.rel_no = col.rel_no
         AND colt.line_item_no = col.line_item_no)  ;
   
   CURSOR Get_StrCode( order_no_ IN VARCHAR2,line_no_ IN VARCHAR2,rel_no_ IN VARCHAR2,line_item_no_ IN NUMBER) IS
      SELECT strcode FROM cust_order_line_taxes_tab
      WHERE order_no = order_no_
      AND line_no = line_no_
      AND rel_no = rel_no_
      AND line_item_no = line_item_no_;
   
   total_charge_ NUMBER := 0;
   str_code_    VARCHAR2(20);
BEGIN
   FOR i IN Get_Order_Lines(contract_,order_no_)
   LOOP
      OPEN Get_StrCode( order_no_, i.line_no, i.rel_no, i.line_item_no);
      FETCH Get_StrCode INTO str_code_;
      CLOSE Get_StrCode;
      total_charge_ := total_charge_+ CUST_ORDER_LINE_TAXES_API.Get_Total_Taxes_Base(
         contract_,
         order_no_,
         i.line_no,
         i.rel_no,
         i.line_item_no,
         str_code_,
         I.sale_unit_price,       -- i.Base_Sale_Unit_Price,
         i.buy_Qty_Due,
         currency_code_);
   END LOOP;
   RETURN nvl(total_charge_,0);
END Get_Total_Charge;

FUNCTION Get_Total_Charge_Curr(
   contract_ 				IN VARCHAR2,
   order_no_				IN VARCHAR2,
   currency_code_         IN VARCHAR2,
	hsn_sac_code_       IN VARCHAR2 DEFAULT NULL
   ) RETURN NUMBER
IS
   CURSOR Get_Order_Lines(contract_ IN VARCHAR2,order_no_ IN VARCHAR2) IS
      SELECT col.line_no,
             col.rel_no,
             col.line_item_no,
             col.base_sale_unit_price,
             col.sale_unit_price,
             col.buy_qty_due*col.price_conv_factor buy_qty_due
      FROM customer_order_line_tab col
      WHERE col.contract = contract_
      AND col.order_no = order_no_
      AND col.rowstate != 'Cancelled'
      AND   ((Nvl(GAdvOnHsn_,'FALSE') = 'FALSE') OR
             (NVL(GAdvOnHsn_,'FALSE') = 'TRUE' AND Nvl(c_hsn_sac_code,'%') LIKE NVL(hsn_sac_code_,'%')))
      AND EXISTS (SELECT 1 FROM cust_order_line_taxes_tab colt
         WHERE colt.order_no = order_no_
         AND colt.line_no = col.line_no
         AND colt.rel_no = col.rel_no
         AND colt.line_item_no = col.line_item_no)  ;
   
   CURSOR Get_StrCode( order_no_ IN VARCHAR2,line_no_ IN VARCHAR2,rel_no_ IN VARCHAR2,line_item_no_ IN NUMBER) IS
      SELECT strcode FROM cust_order_line_taxes_tab
      WHERE order_no = order_no_
      AND line_no = line_no_
      AND rel_no = rel_no_
      AND line_item_no = line_item_no_;
   
   total_charge_ NUMBER := 0;
   str_code_    VARCHAR2(20);
BEGIN
   FOR i IN Get_Order_Lines(contract_,order_no_)
   LOOP
      OPEN Get_StrCode( order_no_, i.line_no, i.rel_no, i.line_item_no);
      FETCH Get_StrCode INTO str_code_;
      CLOSE Get_StrCode;
      total_charge_ := total_charge_+ CUST_ORDER_LINE_TAXES_API.Get_Total_Taxes_Curr1(
         contract_,
         order_no_,
         i.line_no,
         i.rel_no,
         i.line_item_no,
         str_code_,
         i.sale_unit_price,
         i.buy_Qty_Due,
         currency_code_);
   END LOOP;
   RETURN nvl(total_charge_,0);										     
END Get_Total_Charge_Curr;

PROCEDURE Insert_Struct_Cust(
   order_no_ IN VARCHAR2,
   company_ IN VARCHAR2,
   cust_id_ IN VARCHAR2)
IS
   CURSOR C1(c_id_ VARCHAR2,comp_ VARCHAR2) IS 
      SELECT STRUCTURE_ID 
      FROM CUSTOMER_SALES_STRUCTURE
      WHERE CUSTOMER_NO=c_id_ 
      AND COMPANY=comp_ 
      AND ACTIVE_FLAG='TRUE';
   info1_ VARCHAR2(3200);
   objid1_ VARCHAR2(3200);
   objversion1_ VARCHAR2(3200);
   attr1_ VARCHAR2(3200);
BEGIN
   FOR I IN C1(cust_id_,company_)
   LOOP
      Client_SYS.clear_attr(attr1_);
      Client_SYS.Add_To_Attr('ORDER_NO',order_no_,attr1_);
      Client_SYS.Add_To_Attr('COMPANY',company_,attr1_);
      Client_SYS.Add_To_Attr('STRUCTURE_ID',I.STRUCTURE_ID,attr1_);
      cust_order_analysis_head_api.New__(info1_,objid1_,objversion1_,attr1_,'DO');
   END LOOP;
END Insert_Struct_Cust;


PROCEDURE Insert_Struct_Site(
   order_no_ IN VARCHAR2,
   contract_ IN VARCHAR2,
   company_ IN VARCHAR2)
IS
   CURSOR C1(con_ VARCHAR2,comp_ VARCHAR2) IS 
      SELECT STRUCTURE_ID 
      FROM SITE_SALES_STRUCTURE
      WHERE CONTRACT=con_ 
      AND COMPANY=comp_ 
      AND ACTIVE_FLAG='TRUE';
   info1_ VARCHAR2(3200);
   objid1_ VARCHAR2(3200);
   objversion1_ VARCHAR2(3200);
   attr1_ VARCHAR2(3200);
BEGIN
   FOR I IN C1(contract_,company_)
   LOOP
      Client_SYS.clear_attr(attr1_);
      Client_SYS.Add_To_Attr('ORDER_NO',order_no_,attr1_);
      Client_SYS.Add_To_Attr('COMPANY',company_,attr1_);
      Client_SYS.Add_To_Attr('STRUCTURE_ID',I.STRUCTURE_ID,attr1_);
      cust_order_analysis_head_api.New__(info1_,objid1_,objversion1_,attr1_,'DO');
   END LOOP;
END Insert_Struct_Site;

FUNCTION Get_CO_From_PO (
   order_no_ VARCHAR2) RETURN VARCHAR2
IS
   ord_no_  VARCHAR2(12);
   CURSOR get_ord IS
      SELECT order_no 
      FROM customer_order_tab 
      WHERE internal_po_no = order_no_
      AND rowstate <> 'Cancelled' ;
BEGIN
   OPEN get_ord;
   FETCH get_ord INTO ord_no_;
   CLOSE get_ord;
   RETURN ord_no_;
END Get_CO_From_PO;


FUNCTION Get_Gross_Hsn_Value(order_no_ VARCHAR2,
                             hsn_sac_code_ VARCHAR2 DEFAULT NULL) RETURN NUMBER
IS
  ret_value_   NUMBER;
BEGIN
   IF NVL(GAdvOnHsn_,'FALSE') = 'TRUE' THEN
      ret_value_ := Customer_Order_API.Get_Gross_Amount(order_no_, hsn_sac_code_) +   
                    CUSTOMER_ORDER_API.Get_Total_Charge_Curr (Customer_Order_api.Get_Contract(order_no_),
                                                              order_no_,
                                                              Customer_order_api.Get_Currency_Code(order_no_),
                                                              hsn_sac_code_);
   ELSE   
      ret_value_ := Customer_Order_API.Get_Gross_Amount(order_no_) +   
                    CUSTOMER_ORDER_API.Get_Total_Charge_Curr (Customer_Order_api.Get_Contract(order_no_),
                                                              order_no_,
                                                              Customer_order_api.Get_Currency_Code(order_no_)
                                                              );
   END IF;
   RETURN Nvl(ret_value_,0);
END Get_Gross_Hsn_Value;                             


FUNCTION CALC_TAX_AMT(order_no_          VARCHAR2,
                      hsn_sac_code_      VARCHAR2,
                      inclusive_         VARCHAR2,
                      tax_code_          VARCHAR2,
                      tax_type_          VARCHAR2,
                      tax_rate_type_     VARCHAR2,
                      cumm_order_value_  NUMBER,
                      base_amt_          NUMBER,
                      line_tax_amt_      NUMBER,
                      total_adv_amt_     NUMBER,
                      adv_perc_          NUMBER,
                      cumm_tax_perc_     NUMBER,
                      cumm_tax_perc_wo_cess_     NUMBER,
                      tax_perc_          NUMBER,
                      currency_          VARCHAR2,
                      base_tax_          VARCHAR2 DEFAULT 'Tax',
                      show_info_         VARCHAR2 DEFAULT 'FALSE') RETURN NUMBER
IS
  tax_amt_            NUMBER;
  line_adv_amt_       NUMBER;
  gross_up_amt_       NUMBER;
  new_tax_amt_        NUMBER;
  ret_value_          NUMBER;
  curr_rounding_      NUMBER;
  total_tax_          NUMBER;
  total_tax_wo_cess_  NUMBER;
  order_value_        NUMBER:=cumm_order_value_;

  coproratelogic_     VARCHAR2(5):= 'FALSE';
  cess_on_tax_        VARCHAR2(5):= 'FALSE';

BEGIN
   IF Nvl(base_amt_,0) = 0 OR
      Nvl(tax_perc_,0) = 0 OR
      Nvl(cumm_order_value_,0) = 0 OR
      Nvl(total_adv_amt_,0) = 0 OR
      NVL(adv_perc_,0) = 0
      THEN
      ret_value_ := 0;
   ELSE
      curr_rounding_ := Nvl(Currency_Code_Api.Get_Currency_Rounding(Site_Api.Get_Company(Customer_Order_Api.Get_Contract(order_no_)),Nvl(currency_,Customer_Order_Api.Get_Currency_Code(order_no_))),2);
      IF coproratelogic_ = 'FALSE' THEN
-----------------------------------------
----- Full Adv Value Logic --------------
-----------------------------------------
           IF NVL(inclusive_,'FALSE') = 'FALSE' THEN
               IF Nvl(tax_rate_type_,'3') = '1' THEN
                  tax_amt_   := tax_perc_;
               ELSE
                  IF Dist_Tax_Code_Api.Is_GST_Cess(tax_type_,tax_code_) = 'TRUE' AND cess_on_tax_ = 'TRUE' THEN
                     tax_amt_   := (Nvl(total_adv_amt_,0) * Nvl(cumm_tax_perc_wo_cess_ ,0)/100)*tax_perc_/100;
                  ELSE
                     tax_amt_   := Nvl(total_adv_amt_,0) * Nvl(tax_perc_,0)/100;
                  END IF;
               END IF;
               new_tax_amt_   := Round(tax_amt_,curr_rounding_);
               IF base_tax_ = 'Tax' THEN
                  ret_value_     := Nvl(new_tax_amt_,0);
               ELSE
                  IF Dist_Tax_Code_Api.Is_GST_Cess(tax_type_,tax_code_) = 'TRUE' AND cess_on_tax_ = 'TRUE' THEN
                      line_adv_amt_  := (Nvl(total_adv_amt_,0) * Nvl(cumm_tax_perc_wo_cess_ ,0)/100);
                  ELSE
                      line_adv_amt_  := total_adv_amt_ ;
                  END IF;
                  ret_value_     := Round(Nvl(line_adv_amt_,0),curr_rounding_);  -- * 100/Adv_Perc_;
               END IF;
            ELSE
               order_value_       := CUSTOMER_ORDER_API.Get_Gross_Hsn_Value(order_no_,NULL);
               total_tax_         := total_adv_amt_ * cumm_tax_perc_/100;
               total_tax_         := ROUND(total_tax_ * total_adv_amt_/(total_adv_amt_+total_tax_),curr_rounding_);
               gross_up_amt_      := total_adv_amt_  - total_tax_;   -- (Line_Adv_Amt_ * 100)/(100 + Cumm_Tax_Perc_);
               total_tax_wo_cess_ := gross_up_amt_ * cumm_tax_perc_wo_cess_/100;
               IF Nvl(tax_rate_type_,'3') = '1' THEN
                  tax_amt_      := tax_perc_;
               ELSE
                  IF Dist_Tax_Code_Api.Is_GST_Cess(tax_type_,tax_code_) = 'TRUE' AND cess_on_tax_ = 'TRUE' THEN
                     tax_amt_      := ROUND(((Nvl(total_tax_wo_cess_,0) * tax_perc_)/100),curr_rounding_);
                  ELSE
                     tax_amt_      := ROUND(((Nvl(gross_up_amt_,0) * tax_perc_)/100),curr_rounding_);
                  END IF;
               END IF;
               new_tax_amt_   := Round(tax_amt_,curr_rounding_);

               ret_value_     := Nvl(new_tax_amt_,0);
               IF base_tax_ = 'Tax' THEN
                  ret_value_     := Nvl(new_tax_amt_,0);
               ELSE
                  IF Dist_Tax_Code_Api.Is_GST_Cess(tax_type_,tax_code_) = 'TRUE' AND cess_on_tax_ = 'TRUE' THEN
                      ret_value_     := ROUND(total_tax_wo_cess_,curr_rounding_);
                  ELSE
                      ret_value_     := Round(Nvl(gross_up_amt_,0),curr_rounding_);  -- * 100/Adv_Perc_;
                  END IF;
               END IF;
            END IF;
      ELSE
----------------------------------------------------------------
----- Prorate Bases Adv Value Logic on Order Line --------------
----------------------------------------------------------------

         IF NVL(inclusive_,'FALSE') = 'FALSE' THEN
            IF Nvl(tax_rate_type_,'3') = '1' THEN
               tax_amt_   := tax_perc_;
            ELSE
               tax_amt_   := Nvl(base_amt_,0) * Nvl(tax_perc_,0)/100;
   --            Tax_Amt_   := Nvl(Base_Amt_,0) * Total_Adv_Amt_/Order_Value_;
            END IF;
            new_tax_amt_   := tax_amt_;
            IF base_tax_ = 'Tax' THEN
               ret_value_     := Nvl(new_tax_amt_,0);
            ELSE
               line_adv_amt_ := (base_amt_ + line_tax_amt_)*total_adv_amt_/order_value_;
               ret_value_     := Round(Nvl(line_adv_amt_,0),curr_rounding_);  -- * 100/Adv_Perc_;
            END IF;
         ELSE
            IF hsn_sac_code_ IS NOT NULL THEN
               order_value_ := CUSTOMER_ORDER_API.Get_Gross_Hsn_Value(order_no_,hsn_sac_code_);
            END IF;
            line_adv_amt_ := (base_amt_ + line_tax_amt_)*total_adv_amt_/order_value_;
            gross_up_amt_   := (line_adv_amt_ * 100)/(100 + cumm_tax_perc_);
            IF Nvl(tax_rate_type_,'3') = '1' THEN
               tax_amt_      := tax_perc_;
            ELSE
               tax_amt_      := ROUND(((Nvl(gross_up_amt_,0) * tax_perc_)/100-0.005),curr_rounding_);
            END IF;
            new_tax_amt_    := tax_amt_ * 100/adv_perc_;

            ret_value_     := Nvl(new_tax_amt_,0);
            IF base_tax_ = 'Tax' THEN
               ret_value_     := Nvl(new_tax_amt_,0);
            ELSE
               ret_value_     := Round(Nvl(gross_up_amt_,0),curr_rounding_);  -- * 100/Adv_Perc_;
            END IF;
         END IF;
      END IF;
   END IF;
    --- Changed by Adarsh and Chandar 18/12/2018 Start
   IF Dist_Tax_Code_API.Get_Vat_Type(tax_code_) = 'TRUE' THEN        
      ret_value_ := Round(ret_value_,0);
   END IF;    
      --- Changed by Adarsh and Chandar 18/12/2018 Finish   
   IF show_info_ = 'TRUE' THEN
      Dbms_Output.put_line('order_no_                       => '||order_no_);
      Dbms_Output.put_line('hsn_sac_code_                   => '||hsn_sac_code_);
      Dbms_Output.put_line('tax_code_                       => '||tax_code_||'^'||tax_type_||'^'||tax_rate_type_);
      Dbms_Output.put_line('inclusive_                      => '||inclusive_);
      Dbms_Output.put_line('base_tax_                       => '||base_tax_);
      Dbms_Output.put_line('order_value_      (OV)          => '||order_value_);
      Dbms_Output.put_line('total_adv_amt_    (TA)          => '||total_adv_amt_);
      Dbms_Output.put_line('base_amt_         (BA)          => '||base_amt_);
      Dbms_Output.put_line('line_tax_amt_     (LT)          => '||line_tax_amt_);
      Dbms_Output.put_line('Total-Value     TV=>(BA+LT)     => '||(base_amt_ + line_tax_amt_));
      Dbms_Output.put_line('line_adv_amt_  LA=>(TV*TA/OV)   => '||line_adv_amt_);
      Dbms_Output.put_line('tax_perc_         (TP)          => '||tax_perc_||' %');
      Dbms_Output.put_line('cumm_tax_perc_    (CTP)         => '||cumm_tax_perc_||' %  '||cumm_tax_perc_wo_cess_||'%');
      Dbms_Output.put_line('gross_up_amt_  GP=>(LA*100/(100+CTP)  => '||gross_up_amt_);
      Dbms_Output.put_line('tax_amt_       TA=>GP*TP/100    => '||tax_amt_);
      Dbms_Output.put_line('adv_perc_         (AP)          => '||adv_perc_||' %');
      Dbms_Output.put_line('new_tax_amt_  NTA=>TA*100/AP)   => '||new_tax_amt_);
      Dbms_Output.put_line('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');
   END IF;
   RETURN NVL(ret_value_,0);
END CALC_TAX_AMT;                  

PROCEDURE Create_Customer_Order_Advance(pOrder_no_      VARCHAR2,
                                        pHSN_Code_      VARCHAR2,
                                        pAdvance_Date_  DATE DEFAULT SYSDATE,
                                        pAdvance_Amt_   NUMBER DEFAULT NULL,
                                        pAdvance_Perc_  NUMBER DEFAULT NULL,                                                          
                                        pTax_Inclusive_ VARCHAR2 DEFAULT 'TRUE')
IS
   Advance_Date_       DATE := TRUNC(NVL(pAdvance_Date_,SYSDATE));                                   
   Advance_Amt_        NUMBER;
   Advance_Perc_       NUMBER;
   Tax_Inclusive_      VARCHAR2(5):= Nvl(pTax_Inclusive_,'TRUE');
   invoice_id_         VARCHAR2(50);
   tax_attr_           VARCHAR2(2000);
   
   CURSOR cCO IS
   SELECT ORDER_NO,
       CUSTOMER_NO,
       CURRENCY_CODE,
       WANTED_DELIVERY_DATE,
       C_HSN_SAC_CODE,
       Company_Order_Info_API.Get_Base_For_Adv_Invoice(SITE_API.Get_Company(CONTRACT)) Base_Amt_For_Adv,
       PAY_TERM_BASE_DATE,
       PAY_TERM_ID,
       CUSTOMER_NO_PAY,
       Customer_Order_API.Get_Total_Sale_Price__(ORDER_NO,C_HSN_SAC_CODE) Total_Sale_Price,
       Customer_Order_API.Get_Ord_Gross_Amount(ORDER_NO,C_HSN_SAC_CODE) Gross_Amount,
       Customer_Order_Inv_Head_API.Get_Co_Inv_Net_Total(SITE_API.Get_Company(CONTRACT),ORDER_NO) Net_Total,
       Customer_Order_Inv_Head_API.Get_Ad_Gro_Without_Invoice_Fee(SITE_API.Get_Company(CONTRACT),ORDER_NO) Adv_Gro_Wo_Inv_Fee,
       CONTRACT,
       SITE_API.Get_Company(CONTRACT) Company,
       Customer_Order_API.Get_Total_Sale_Charge__(ORDER_NO) Total_Sale_Charge,
       CUSTOMER_ORDER_API.Get_Tot_Charge_Sale_Tax_Amt(ORDER_NO) Total_Charge_Sale_Tax_Amt,
       Company_Order_Info_API.Get_Base_For_Adv_Invoice_Db(SITE_API.Get_Company(CONTRACT)) Base_For_Adv_Inv_Db,
       IDENTITY_INVOICE_INFO_API.Get_Def_Vat_Code(SITE_API.Get_Company(CONTRACT),
                                                         CUSTOMER_NO,
                                                         Customer_Info_API.Get_Party_Type(CUSTOMER_NO)) Def_Vat_Code,
       Site_API.Get_Site_Date(CONTRACT),
       Customer_Order_Inv_Head_API.Get_Co_Gross_Total(SITE_API.Get_Company(CONTRACT),ORDER_NO) Gross_Total,
       Customer_Order_Inv_Head_API.Get_Co_Inv_Net_Total(SITE_API.Get_Company(CONTRACT),ORDER_NO) Inv_Net_Total,
       Customer_Order_Inv_Head_API.Get_Co_Inv_Gross_Total(SITE_API.Get_Company(CONTRACT),ORDER_NO) Inv_Gross_Total,
       SHIP_ADDR_NO,
       SUPPLY_COUNTRY_DB
  FROM CUSTOMER_ORDER t
   WHERE  t.order_no = pOrder_no_;
   
   Co_rec_   cCO%ROWTYPE;
   
   Flag_   VARCHAR2(5);
   CURSOR cCOT IS
      SELECT FEE_CODE,
             STATUTORY_FEE_API.Get_Description(COMPANY, FEE_CODE),
             TAX_PERCENTAGE,
             TAX_RATE_TYPE,
             SUM(BASE_TAX_AMT) BASE_TAX_AMT,
             SUM(TAX_AMOUNT) * DECODE(SOURCE, 'IND-LOC', 1, 10 / 100) TAX_AMOUNT
  FROM (SELECT 'IND-LOC' SOURCE,
                COMPANY,
                FEE_CODE,
                STATUTORY_FEE_API.Get_Description(COMPANY, FEE_CODE),
                TAX_PERCENTAGE,
                TAX_TYPE,
                TAX_RATE_TYPE,
                CUSTOMER_ORDER_API.CALC_TAX_AMT(ORDER_NO,
                                                       C_HSN_SAC_CODE,
                                                       Nvl(Tax_Inclusive_,'TRUE'),
                                                       FEE_CODE,
                                                       TAX_TYPE,
                                                       TAX_RATE_TYPE,
                                                       TOTAL_VALUE,
                                                       BASE_AMT,
                                                       TAX_AMT,
                                                       Advance_Amt_,
                                                       Advance_Perc_,
                                                       CUMM_TAX_PERC,
                                                       CUMM_TAX_RATE,
                                                       TAX_PERCENTAGE,
                                                       CUSTOMER_ORDER_API.Get_Currency_Code(ORDER_NO),
                                                       'Base') BASE_TAX_AMT,
                CUSTOMER_ORDER_API.CALC_TAX_AMT(ORDER_NO,
                                                       C_HSN_SAC_CODE,
                                                       Nvl(Tax_Inclusive_,'TRUE'),
                                                       FEE_CODE,
                                                       TAX_TYPE,
                                                       TAX_RATE_TYPE,
                                                       TOTAL_VALUE,
                                                       BASE_AMT,
                                                       TAX_AMT,
                                                       Advance_Amt_,
                                                       Advance_Perc_,
                                                       CUMM_TAX_PERC,
                                                       CUMM_TAX_RATE,
                                                       TAX_PERCENTAGE,
                                                       CUSTOMER_ORDER_API.Get_Currency_Code(ORDER_NO),
                                                       'Tax') TAX_AMOUNT
           FROM CUST_ORDER_LINE_TAXES_HSN
          WHERE ORDER_NO = pOrder_no_
            AND C_HSN_SAC_CODE = pHSN_Code_)
 GROUP BY SOURCE,
          FEE_CODE,
          STATUTORY_FEE_API.Get_Description(COMPANY, FEE_CODE),
          TAX_TYPE,
          TAX_RATE_TYPE,
          TAX_PERCENTAGE
 ORDER BY TAX_TYPE,
          FEE_CODE,
          TAX_PERCENTAGE;

BEGIN
   OPEN  cCO;
   FETCH cCO INTO Co_rec_;
   CLOSE cCO;
   IF Co_Rec_.Order_No IS NOT NULL AND Co_Rec_.Gross_Amount > 0 THEN
      IF pAdvance_Amt_ IS NOT NULL OR pAdvance_Perc_ IS NOT NULL THEN
         IF pAdvance_Amt_ IS NOT NULL THEN
            Advance_Amt_  := pAdvance_Amt_;
            Advance_Perc_ := Round(Advance_Amt_ * 100 /Co_Rec_.Gross_Amount,0);
         ELSE
            Advance_Amt_  := Round(Co_Rec_.Gross_Amount * pAdvance_Perc_/100, 0);
            Advance_Perc_ := pAdvance_Perc_;
         END IF;   
      END IF;   
      Client_sys.Clear_Attr(tax_attr_);
      IF Advance_Amt_ > 0 THEN
         Flag_ := 'FALSE';
         FOR COT_Rec_ IN cCOT
         LOOP   
            Flag_ := 'TRUE';
            Client_Sys.Add_To_Attr('SALETX',              COT_Rec_.Fee_Code,            tax_attr_);
            Client_Sys.Add_To_Attr('TAXABLE_AMOUNT',      COT_Rec_.Base_Tax_Amt,        tax_attr_);
            Client_Sys.Add_To_Attr('TAX_PERCENTAGE',      COT_Rec_.Tax_Percentage,      tax_attr_);
            Client_Sys.Add_To_Attr('AMOUNT',              COT_Rec_.Tax_Amount,          tax_attr_);
         END LOOP;   
         Dbms_Output.put_line('Advance_Amt_   => '||Advance_Amt_);
         Dbms_Output.put_line('Advance_Perc_  => '||Advance_Perc_);
         Dbms_Output.put_line('Advance_Date_  => '||To_Char(Advance_Date_,'DD/MM/YYYY'));
         Dbms_Output.put_line('pHSN_Code_     => '||pHSN_Code_);
         Dbms_Output.put_line('Tax_Inclusive_ => '||Tax_Inclusive_);
         Dbms_Output.put_line('tax_attr_      => '||tax_attr_);
         IF tax_attr_ IS NOT NULL /*AND 1=2*/ THEN
            invoice_id_ := NULL;
            Invoice_Customer_Order_API.Create_Advance_Invoice__(invoice_id_             => invoice_id_, 
                                                                order_no_               => CO_Rec_.order_no, 
                                                                adv_pay_amt_            => Advance_Amt_, 
                                                                tax_msg_               => tax_attr_, 
                                                                invoice_text_           => 'Pre Payment', 
                                                                pay_base_date_          => Advance_Date_, 
                                                                pay_term_id_            => Co_rec_.PAY_TERM_ID, 
                                                                pay_tax_                => 'TRUE');
            
           DBMS_OUTPUT.put_line('Invoice-Id: '||invoice_id_||'   Invoice-No '||Invoice_api.Get_Invoice_No(CO_Rec_.company,invoice_id_));
         END IF;
      END IF;   
   END IF;   
END Create_Customer_Order_Advance;

----(-/+)  310823 RiLoIn   <C_DML_Code_Correction> start

PROCEDURE Set_Rowstate (
   order_no_  IN VARCHAR2,
   state_     IN     VARCHAR2 )
IS
rec_    customer_order_tab%ROWTYPE;
BEGIN
 rec_ := Get_Object_By_Keys___(order_no_);
 Finite_State_Set___(rec_,state_);
END Set_Rowstate;
----(-/+)  310823 RiLoIn   <C_DML_Code_Correction> end
--(+) 231121 TecVaruSa MERGE-ENVECON-1 (FINISH)
