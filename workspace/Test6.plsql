-----------------------------------------------------------------------------
--
--  Logical unit: CSupplierOnboarding
--  Component:    CMOD
--
--  IFS Developer Studio Template Version 3.0.1
--
--  Date    Sign       History
--  ------  ------     ---------------------------------------------------------
--  231229  TecPardhY  BLK-IMM-15-M: Added Check_Bank_Account_No and modified Check_Common___, Check_Mandatory_Fields
--  220921  TecPardhY  BLK-IMM-15-M: Dependency corrections and rework corrections
--  220921  TecPardhY  BLK-IMM-15-M: Created
------------------------------------------------------------------------------------

layer Cust;

-------------------- PUBLIC DECLARATIONS ------------------------------------


-------------------- PRIVATE DECLARATIONS -----------------------------------


-------------------- LU SPECIFIC IMPLEMENTATION METHODS ---------------------
--(+) 220921 TecPardhY BLK-IMM-15-M (START)
@Override
PROCEDURE Prepare_Insert___ (
   attr_ IN OUT VARCHAR2 )
IS
BEGIN
   super(attr_);
   Client_SYS.Add_To_Attr('RECORD_CREATED_BY',   Fnd_Session_API.Get_Fnd_User, attr_);
   Client_SYS.Add_To_Attr('RECORD_CREATED_DATE', SYSDATE,                      attr_);
   
END Prepare_Insert___;

@Override
PROCEDURE Insert___ (
   objid_      OUT    VARCHAR2,
   objversion_ OUT    VARCHAR2,
   newrec_     IN OUT c_supplier_onboarding_tab%ROWTYPE,
   attr_       IN OUT VARCHAR2 )
IS
BEGIN
   newrec_.ifs_supplier_id  := NULL; 
   newrec_.temp_supplier_id := 'T' || lpad (c_temp_supplier_id_seq.nextval, 6, '0');
   Client_SYS.Add_To_Attr('TEMP_SUPPLIER_ID', newrec_.temp_supplier_id, attr_);
   
   newrec_.record_created_by   := Fnd_Session_API.Get_Fnd_User;
   newrec_.record_created_date := sysdate;
   
   super(objid_, objversion_, newrec_, attr_);
END Insert___;

@Override
PROCEDURE Check_Common___ (
   oldrec_ IN     c_supplier_onboarding_tab%ROWTYPE,
   newrec_ IN OUT c_supplier_onboarding_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
   count_         NUMBER;
   
   CURSOR get_record_count IS
      SELECT count(0)
      FROM   c_supplier_onboarding_tab
      WHERE  supplier_email     =  newrec_.supplier_email
      AND    registration_for   =  newrec_.registration_for
      AND    temp_supplier_id  != NVL(newrec_.temp_supplier_id, '*#$');
      
   CURSOR get_pan_or_duns_count IS
      SELECT count(0)
      FROM   c_supplier_onboarding_tab
      WHERE  registration_for         =  newrec_.registration_for
      AND    NVL(pan_no, duns_number) =  NVL(newrec_.pan_no, newrec_.duns_number)
      AND    temp_supplier_id        != NVL(newrec_.temp_supplier_id, '*#$');
   
   CURSOR get_e_proc_count IS
      SELECT count(0)
      FROM   c_supplier_onboarding_tab
      WHERE  registration_for   =  newrec_.registration_for
      AND    e_procurement_id   =  newrec_.e_procurement_id
      AND    temp_supplier_id  !=  NVL(newrec_.temp_supplier_id, '*#$');
BEGIN
   OPEN  get_record_count;
   FETCH get_record_count INTO count_;
   CLOSE get_record_count;
   
   IF count_ > 0 THEN
      Error_SYS.Record_Exist(lu_name_, 'ERRMAILREG: A record already exist with the given supplier email '':P1'' and registration for '':P2''', newrec_.supplier_email, newrec_.registration_for);
   END IF;
   
   OPEN  get_pan_or_duns_count;
   FETCH get_pan_or_duns_count INTO count_;
   CLOSE get_pan_or_duns_count;
   
   IF count_ > 0 AND (newrec_.pan_no IS NOT NULL OR newrec_.duns_number IS NOT NULL) THEN
      Error_SYS.Record_Exist(lu_name_, 'ERRPANDUP: A record already exist with the given Pan No/DUNS No '':P1'' and registration for '':P2''', NVL(newrec_.pan_no, newrec_.duns_number), newrec_.registration_for);
   END IF;
   
   OPEN  get_e_proc_count;
   FETCH get_e_proc_count INTO count_;
   CLOSE get_e_proc_count;
   
   IF count_ > 0 AND newrec_.e_procurement_id IS NOT NULL THEN
      Error_SYS.Record_Exist(lu_name_, 'ERRPANDUP: A record already exist with the given E-Procurement Id '':P1'' and registration for '':P2''', newrec_.e_procurement_id, newrec_.registration_for);
   END IF;
   
   --(+) 220921 TecRahulM BLK-IMM-15-M (START)   
   Validate_In_Pincode(newrec_.office_country ,newrec_.office_zip_code);
   Validate_In_Pincode(newrec_.work_country ,newrec_.work_zip_code);
   --(+) 220921 TecRahulM BLK-IMM-15-M (FINISH)
   
   IF newrec_.pan_not_available = 'N' AND Validate_Pan_No(newrec_.pan_no) = false THEN
      Error_SYS.Record_General(lu_name_, 'ERRPANNO: Pan No is not in correct format');
   END IF;
   
   IF newrec_.same_as_office = 'Y' THEN
      newrec_.work_address1 := newrec_.office_address1;
      newrec_.work_address2 := newrec_.office_address2;
      newrec_.work_address3 := newrec_.office_address3;
      newrec_.work_address4 := newrec_.office_address4;
      newrec_.work_address5 := newrec_.office_address5;
      newrec_.work_address6 := newrec_.office_address6;
      newrec_.work_zip_code := newrec_.office_zip_code;
      newrec_.work_city     := newrec_.office_city;
      newrec_.work_county   := newrec_.office_county;  
      newrec_.work_state    := newrec_.office_state;
      newrec_.work_country  := newrec_.office_country;
   END IF;
   
   Check_Bank_Account_No(newrec_.bank_account_no, newrec_.supplier_email, newrec_.pan_no, newrec_.duns_number);
   
   super(oldrec_, newrec_, indrec_, attr_);
END Check_Common___;

@Override
PROCEDURE Check_Update___ (
   oldrec_ IN     c_supplier_onboarding_tab%ROWTYPE,
   newrec_ IN OUT c_supplier_onboarding_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
BEGIN
   newrec_.last_modified_by := Fnd_Session_API.Get_Fnd_User;
   Client_SYS.Add_To_Attr('LAST_MODIFIED_BY', newrec_.last_modified_by, attr_);
   
   super(oldrec_, newrec_, indrec_, attr_);
END Check_Update___;

PROCEDURE Do_Create_Supplier___ (
   rec_  IN OUT NOCOPY c_supplier_onboarding_tab%ROWTYPE,
   attr_ IN OUT NOCOPY VARCHAR2 )
IS
BEGIN
   Create_Ifs_Supplier(NULL, rec_);
END Do_Create_Supplier___;

@Override
PROCEDURE Set_Verified__ (
   info_       OUT    VARCHAR2,
   objid_      IN     VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   attr_       IN OUT VARCHAR2,
   action_     IN     VARCHAR2 )
IS
   c_state_            c_supplier_onboarding_tab.rowstate%TYPE;
   c_attr_             VARCHAR2(32000); 
   c_temp_supplier_id_ c_supplier_onboarding_tab.temp_supplier_id%TYPE;
   
   CURSOR get_keys IS
      SELECT  temp_supplier_id
      FROM    c_supplier_onboarding_tab
      WHERE   ROWID = objid_;
BEGIN
   super(info_, objid_, objversion_, attr_, action_);
   
   c_state_ := Client_SYS.Get_Item_Value('__OBJSTATE', attr_);
   
   IF c_state_ = 'Verified' THEN
      Client_SYS.Clear_Attr(c_attr_);
      Client_SYS.Add_To_Attr('VERIFIED_DATE', sysdate,                      c_attr_); 
      Client_SYS.Add_To_Attr('VERIFIED_BY',   Fnd_Session_API.Get_Fnd_User, c_attr_); 
      
      OPEN  get_keys;
      FETCH get_keys INTO c_temp_supplier_id_;
      CLOSE get_keys;
      
      C_Modify(c_temp_supplier_id_, c_attr_, 'DO');
   END IF;
END Set_Verified__;

@Override
PROCEDURE Set_Hold__ (
   info_       OUT    VARCHAR2,
   objid_      IN     VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   attr_       IN OUT VARCHAR2,
   action_     IN     VARCHAR2 )
IS
   state_              c_supplier_onboarding_tab.rowstate%TYPE;
   c_attr_             VARCHAR2(32000);
   c_temp_supplier_id_ c_supplier_onboarding_tab.temp_supplier_id%TYPE;
   
   CURSOR get_keys IS
      SELECT  temp_supplier_id
      FROM    c_supplier_onboarding_tab
      WHERE   ROWID = objid_;
BEGIN
   super(info_, objid_, objversion_, attr_, action_);
   
   state_ := Client_SYS.Get_Item_Value('__OBJSTATE', attr_);
   
   IF state_ = 'Hold' THEN
      Client_SYS.Clear_Attr(c_attr_);
      Client_SYS.Add_To_Attr('ONHOLD_DATE', sysdate,                      c_attr_); 
      Client_SYS.Add_To_Attr('ONHOLD_BY',   Fnd_Session_API.Get_Fnd_User, c_attr_); 
      
      OPEN  get_keys;
      FETCH get_keys INTO c_temp_supplier_id_;
      CLOSE get_keys;
      
      C_Modify(c_temp_supplier_id_, c_attr_, 'DO');
   END IF;
END Set_Hold__;

@Override
PROCEDURE Check_Delete___ (
   remrec_ IN c_supplier_onboarding_tab%ROWTYPE )
IS
BEGIN
   IF remrec_.rowstate != 'New' THEN
      Error_SYS.Record_General(lu_name_, 'ERRRECDEL: Only records in new status are allowed to delete.');
   END IF;
   
   super(remrec_);
END Check_Delete___;




-------------------- LU SPECIFIC PRIVATE METHODS ----------------------------


-------------------- LU SPECIFIC PROTECTED METHODS --------------------------


-------------------- LU SPECIFIC PUBLIC METHODS -----------------------------


-------------------- LU CUST NEW METHODS -------------------------------------
--(+) 220921 TecPardhY BLK-IMM-15-M (START)
PROCEDURE Create_Ifs_Supplier(
   temp_supplier_id_ IN VARCHAR2,
   rec_              IN c_supplier_onboarding_tab%ROWTYPE)
IS
   temp_supp_rec_           c_supplier_onboarding_tab%ROWTYPE;
   country_db_              supplier_info_tab.country%TYPE;
   generated_supplier_id_   supplier_info_tab.supplier_id%TYPE;
   temp_sup_id_             c_supplier_onboarding_tab.temp_supplier_id%TYPE;
   info_                    VARCHAR2(32000);
   objid_                   VARCHAR2(100);
   objversion_              VARCHAR2(100);
   attr_                    VARCHAR2(32000);
   addr_attr_               VARCHAR2(32000);
   addr_type_attr_          VARCHAR2(32000);
   default_comm_attr_       VARCHAR2(32000);
   comm_attr_               VARCHAR2(32000);
   bank_attr_               VARCHAR2(32000);
   
   CURSOR get_sup_contacts(related_to_ IN VARCHAR2) IS
      SELECT contact_person_name,
             designation,
             telephone_no,
             mobile_no,
             e_mail,
             website
      FROM   c_supplier_onboard_comm_tab
      WHERE  temp_supplier_id = temp_sup_id_
      AND    related_to       = related_to_;
    
   CURSOR get_docs IS
      SELECT doc_class,
             doc_no,
             doc_sheet,
             doc_rev
      FROM   doc_reference_object_tab
      WHERE  lu_name   = 'CSupplierOnboarding'
      AND    key_value = temp_sup_id_ || '^';
   
   CURSOR get_duplicate(email_id_ IN VARCHAR2, pan_no_ IN VARCHAR2, duns_number_ IN VARCHAR2) IS
      SELECT ifs_supplier_id 
      FROM   c_supplier_onboarding_tab t
      WHERE  t.supplier_email = email_id_
      AND    ((pan_no_ IS NULL AND pan_no IS NULL) OR pan_no = pan_no_) 
      AND    ((duns_number_ IS NULL AND duns_number IS NULL) OR duns_number = duns_number_)
      AND    t.rowstate       = 'Created';
BEGIN
   IF rec_.temp_supplier_id IS NOT NULL THEN
      temp_supp_rec_ := rec_;
      temp_sup_id_   := rec_.temp_supplier_id;
   ELSE
      temp_sup_id_   := temp_supplier_id_;
      temp_supp_rec_ := Get_Object_By_Keys___(temp_sup_id_);   
   END IF;
   
   OPEN  get_duplicate(rec_.supplier_email, rec_.pan_no, rec_.duns_number);
   FETCH get_duplicate INTO generated_supplier_id_;
   CLOSE get_duplicate;
   
   IF generated_supplier_id_ IS NULL THEN
      IF temp_supp_rec_.country_origin = 'I' THEN
         country_db_ := 'IN';
      ELSE
         country_db_ := temp_supp_rec_.country_code;
      END IF;
      
      Client_SYS.Clear_Attr(attr_);
      
      --Supplier Header & General Information
      Supplier_Info_API.C_New(attr_, 'PREPARE');
      
      Client_SYS.Add_To_Attr('NAME',                 temp_supp_rec_.supplier_name, attr_);
      Client_SYS.Add_To_Attr('COUNTRY_DB',           country_db_,                  attr_);
      Client_SYS.Add_To_Attr('PARTY_TYPE_DB',        'SUPPLIER',                   attr_);
      Client_SYS.Add_To_Attr('CORPORATE_FORM',       temp_supp_rec_.ownership,     attr_);
      Client_SYS.Add_To_Attr('SUPPLIER_CATEGORY_DB', 'SUPPLIER',                   attr_);
      Client_SYS.Add_To_Attr('C_STATUS_DB',          'NEW',                        attr_);
      Client_SYS.Add_To_Attr('C_DIVISION_APPROVED',  'TRUE',                       attr_);
      
      IF temp_supp_rec_.is_registered = 'Y' THEN
         Client_SYS.Add_To_Attr('C_IS_REGISTERED', 'RS', attr_);
      ELSE
         Client_SYS.Add_To_Attr('C_IS_REGISTERED', 'URS', attr_);
      END IF;
      
      Supplier_Info_API.C_New(attr_, 'DO');
      generated_supplier_id_ := Client_SYS.Get_Item_Value('SUPPLIER_ID', attr_);
      
      --Supplier Address: Registered Office
      Client_SYS.Clear_Attr(addr_attr_);
      Supplier_Info_Address_API.C_New(addr_attr_, 'PREPARE');
      
      Client_SYS.Add_To_Attr('SUPPLIER_ID',   generated_supplier_id_,         addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS_ID',    '10',                           addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS1',      temp_supp_rec_.office_address1, addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS2',      temp_supp_rec_.office_address2, addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS3',      temp_supp_rec_.office_address3, addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS4',      temp_supp_rec_.office_address4, addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS5',      temp_supp_rec_.office_address5, addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS6',      temp_supp_rec_.office_address6, addr_attr_);
      Client_SYS.Add_To_Attr('ZIP_CODE',      temp_supp_rec_.office_zip_code, addr_attr_);
      Client_SYS.Add_To_Attr('CITY',          temp_supp_rec_.office_city,     addr_attr_);
      Client_SYS.Add_To_Attr('COUNTY',        temp_supp_rec_.office_county,   addr_attr_);
      Client_SYS.Add_To_Attr('STATE',         temp_supp_rec_.office_state,    addr_attr_);
      Client_SYS.Add_To_Attr('COUNTRY_DB',    temp_supp_rec_.office_country,  addr_attr_);
      Client_SYS.Add_To_Attr('PARTY_TYPE_DB', 'SUPPLIER',                     addr_attr_);
      Client_SYS.Add_To_Attr('C_EXTENSION_NO', '0',                           addr_attr_);

      Supplier_Info_Address_API.C_New(addr_attr_, 'DO');
      
      --Supplier Address Type : Document/Invoice
      Client_SYS.Clear_Attr(addr_type_attr_);
      Client_SYS.Add_To_Attr('SUPPLIER_ID',          generated_supplier_id_, addr_type_attr_);
      Client_SYS.Add_To_Attr('ADDRESS_ID',           '10',                   addr_type_attr_);
      Client_SYS.Add_To_Attr('ADDRESS_TYPE_CODE_DB', 'INVOICE',              addr_type_attr_);
      Client_SYS.Add_To_Attr('DEF_ADDRESS',          'TRUE',                 addr_type_attr_);  
      
      Supplier_Info_Address_Type_API.C_New(addr_type_attr_, 'DO');
      
      --Creating Registered Office Address Related Contacts
      FOR comm_rec_ IN get_sup_contacts('OFFICE') LOOP
         Client_SYS.Clear_Attr(default_comm_attr_);
         
         Comm_Method_API.C_New(default_comm_attr_, 'PREPARE');
         
         Client_SYS.Add_To_Attr('IDENTITY',      generated_supplier_id_,        default_comm_attr_);
         Client_SYS.Add_To_Attr('PARTY_TYPE_DB', 'SUPPLIER',                    default_comm_attr_);
         Client_SYS.Add_To_Attr('ADDRESS_ID',    '10',                          default_comm_attr_);
         Client_SYS.Add_To_Attr('NAME',          comm_rec_.contact_person_name, default_comm_attr_); 
         Client_SYS.Add_To_Attr('DESCRIPTION',   comm_rec_.designation,         default_comm_attr_); 
         
         IF comm_rec_.telephone_no IS NOT NULL THEN
            comm_attr_ := default_comm_attr_;
            
            Client_SYS.Add_To_Attr('VALUE',        comm_rec_.telephone_no, comm_attr_); 
            Client_SYS.Add_To_Attr('METHOD_ID_DB', 'PHONE',                comm_attr_);
            
            Comm_Method_API.C_New( comm_attr_, 'DO');
         END IF;
         
         IF comm_rec_.mobile_no IS NOT NULL THEN
            comm_attr_ := default_comm_attr_;
            
            Client_SYS.Add_To_Attr('VALUE',        comm_rec_.mobile_no, comm_attr_); 
            Client_SYS.Add_To_Attr('METHOD_ID_DB', 'MOBILE',            comm_attr_);
            
            Comm_Method_API.C_New(comm_attr_, 'DO');
         END IF;
         
         IF comm_rec_.e_mail IS NOT NULL THEN
            comm_attr_ := default_comm_attr_;
            
            Client_SYS.Add_To_Attr('VALUE',        comm_rec_.e_mail, comm_attr_); 
            Client_SYS.Add_To_Attr('METHOD_ID_DB', 'E_MAIL',         comm_attr_);
            
            Comm_Method_API.C_New(comm_attr_, 'DO');
         END IF;
         
         IF comm_rec_.website IS NOT NULL THEN
            comm_attr_ := default_comm_attr_;
            
            Client_SYS.Add_To_Attr('VALUE',        comm_rec_.website, comm_attr_); 
            Client_SYS.Add_To_Attr('METHOD_ID_DB', 'WWW',             comm_attr_);
            
            Comm_Method_API.C_New(comm_attr_, 'DO');
         END IF;
      END LOOP;
      
      --Supplier Bank Details
      IF temp_supp_rec_.bank_account_no IS NOT NULL
      THEN   
         Client_SYS.Clear_Attr(bank_attr_);
         Client_SYS.Add_To_Attr('SUPPLIER_ID',       generated_supplier_id_,           bank_attr_);
         Client_SYS.Add_To_Attr('ADDRESS_ID',        '10',                             bank_attr_);
         Client_SYS.Add_To_Attr('BANK_ACCOUNT_NO',   temp_supp_rec_.bank_account_no,   bank_attr_); 
         Client_SYS.Add_To_Attr('BANK_NAME',         temp_supp_rec_.bank_name,         bank_attr_); 
         Client_SYS.Add_To_Attr('BANK_BRANCH_NAME',  temp_supp_rec_.bank_branch_name,  bank_attr_); 
         Client_SYS.Add_To_Attr('BANK_CITY',         temp_supp_rec_.bank_city,         bank_attr_);
         Client_SYS.Add_To_Attr('BANK_ACCOUNT_TYPE', temp_supp_rec_.bank_account_type, bank_attr_);
         Client_SYS.Add_To_Attr('BANK_IFSC_CODE',    temp_supp_rec_.bank_ifsc_code,    bank_attr_);
         Client_SYS.Add_To_Attr('STATUS',            'Active',                         bank_attr_);
         Client_SYS.Add_To_Attr('BANK_CURRENCY',     temp_supp_rec_.bank_currency,     bank_attr_);
      
         C_Supplier_Bank_Details_API.C_New(bank_attr_, 'DO');
      END IF;
      --Supplier Address : Work Facility
      Client_SYS.Clear_Attr(addr_attr_);
      Supplier_Info_Address_API.C_New(addr_attr_, 'PREPARE');
      
      Client_SYS.Add_To_Attr('SUPPLIER_ID',    generated_supplier_id_,       addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS_ID',     '20',                         addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS1',       temp_supp_rec_.work_address1, addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS2',       temp_supp_rec_.work_address2, addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS3',       temp_supp_rec_.work_address3, addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS4',       temp_supp_rec_.work_address4, addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS5',       temp_supp_rec_.work_address5, addr_attr_); 
      Client_SYS.Add_To_Attr('ADDRESS6',       temp_supp_rec_.work_address6, addr_attr_);
      Client_SYS.Add_To_Attr('ZIP_CODE',       temp_supp_rec_.work_zip_code, addr_attr_);
      Client_SYS.Add_To_Attr('CITY',           temp_supp_rec_.work_city,     addr_attr_);
      Client_SYS.Add_To_Attr('COUNTY',         temp_supp_rec_.work_county,   addr_attr_);
      Client_SYS.Add_To_Attr('STATE',          temp_supp_rec_.work_state,    addr_attr_);
      Client_SYS.Add_To_Attr('COUNTRY_DB',     temp_supp_rec_.work_country,  addr_attr_);
      Client_SYS.Add_To_Attr('PARTY_TYPE_DB',  'SUPPLIER',                   addr_attr_);
      Client_SYS.Add_To_Attr('C_EXTENSION_NO', '0',                         addr_attr_);
      
      Supplier_Info_Address_API.C_New(addr_attr_, 'DO');
      
      --Supplier Address Type : Delivery
      Client_SYS.Clear_Attr(addr_type_attr_);
      Client_SYS.Add_To_Attr('SUPPLIER_ID',          generated_supplier_id_, addr_type_attr_);
      Client_SYS.Add_To_Attr('ADDRESS_ID',           '20',                   addr_type_attr_);
      Client_SYS.Add_To_Attr('ADDRESS_TYPE_CODE_DB', 'DELIVERY',             addr_type_attr_);
      Client_SYS.Add_To_Attr('DEF_ADDRESS',          'TRUE',                 addr_type_attr_);  
      
      Supplier_Info_Address_Type_API.C_New(addr_type_attr_, 'DO');
      
      
      --Creating Work Facility Address Related Contacts
      FOR comm_rec_ IN get_sup_contacts('WORK') LOOP
         Client_SYS.Clear_Attr(default_comm_attr_);
         
         Comm_Method_API.C_New(default_comm_attr_, 'PREPARE');
         
         Client_SYS.Add_To_Attr('IDENTITY',   generated_supplier_id_,        default_comm_attr_);
         Client_SYS.Add_To_Attr('PARTY_TYPE_DB', 'SUPPLIER',                    default_comm_attr_);
         Client_SYS.Add_To_Attr('ADDRESS_ID',    '20',                          default_comm_attr_);
         Client_SYS.Add_To_Attr('NAME',          comm_rec_.contact_person_name, default_comm_attr_); 
         Client_SYS.Add_To_Attr('DESCRIPTION',   comm_rec_.designation,         default_comm_attr_); 
         
         IF comm_rec_.telephone_no IS NOT NULL THEN
            comm_attr_ := default_comm_attr_;
            
            Client_SYS.Add_To_Attr('VALUE',        comm_rec_.telephone_no, comm_attr_); 
            Client_SYS.Add_To_Attr('METHOD_ID_DB', 'PHONE',                comm_attr_);
            
            Comm_Method_API.C_New(comm_attr_, 'DO');
         END IF;
         
         IF comm_rec_.mobile_no IS NOT NULL THEN
            comm_attr_ := default_comm_attr_;
            
            Client_SYS.Add_To_Attr('VALUE',        comm_rec_.mobile_no, comm_attr_); 
            Client_SYS.Add_To_Attr('METHOD_ID_DB', 'MOBILE',            comm_attr_);
            
            Comm_Method_API.C_New(comm_attr_, 'DO');
         END IF;
         
         IF comm_rec_.e_mail IS NOT NULL THEN
            comm_attr_ := default_comm_attr_;
            
            Client_SYS.Add_To_Attr('VALUE',        comm_rec_.e_mail, comm_attr_); 
            Client_SYS.Add_To_Attr('METHOD_ID_DB', 'E_MAIL',         comm_attr_);
            
            Comm_Method_API.C_New(comm_attr_, 'DO');
         END IF;
         
         IF comm_rec_.website IS NOT NULL THEN
            comm_attr_ := default_comm_attr_;
            
            Client_SYS.Add_To_Attr('VALUE',        comm_rec_.website, comm_attr_); 
            Client_SYS.Add_To_Attr('METHOD_ID_DB', 'WWW',             comm_attr_);
            
            Comm_Method_API.C_New(comm_attr_, 'DO');
         END IF;
      END LOOP;
   END IF;
   
   --Connecting documents to new IFS supplier
   FOR doc_rec_ IN get_docs LOOP
      Doc_Reference_Object_API.Create_New_Reference(doc_rec_.doc_class,
                                                    doc_rec_.doc_no ,
                                                    doc_rec_.doc_sheet,
                                                    doc_rec_.doc_rev,
                                                    'SupplierInfoGeneral',
                                                    'SUPPLIER_ID=' || generated_supplier_id_ || '^');
   END LOOP;
   
   --Updating Supplier ID in Supplier Onboarding
   Client_SYS.Clear_Attr(attr_);
   Client_SYS.Add_To_Attr('IFS_SUPPLIER_ID',         generated_supplier_id_,       attr_); 
   Client_SYS.Add_To_Attr('SUPPLIER_CREATED_DATE',   sysdate,                      attr_); 
   Client_SYS.Add_To_Attr('IFS_SUPPLIER_CREATED_BY', Fnd_Session_API.Get_Fnd_User, attr_); 
   
   C_Modify(temp_sup_id_, attr_, 'DO');
END Create_Ifs_Supplier;

FUNCTION Get_Temp_Supplier_Id(
   supplier_id_ IN VARCHAR2) RETURN VARCHAR2
IS
   temp_supplier_id_       c_supplier_onboarding_tab.temp_supplier_id%TYPE;
   
   CURSOR get_supplier_id IS
      SELECT DISTINCT temp_supplier_id 
      FROM   c_supplier_onboarding_tab 
      WHERE  ifs_supplier_id = supplier_id_;
BEGIN
   OPEN  get_supplier_id;
   FETCH get_supplier_id INTO temp_supplier_id_;
   CLOSE get_supplier_id;
   
   RETURN temp_supplier_id_;
END Get_Temp_Supplier_Id; 


PROCEDURE Set_Hold (
   temp_supplier_id_ IN VARCHAR2,
   onhold_remarks_   IN VARCHAR2)
IS
   attr_             VARCHAR2(32000);
   objid_            VARCHAR2(100);
   objversion_       VARCHAR2(100);
   info_             VARCHAR2(2000);
BEGIN
   IF onhold_remarks_ IS NULL THEN
      Error_SYS.Record_General(lu_name_, 'ERRHOLDRES: Hold Remarks should not be empty.');
   END IF;

   Client_SYS.Clear_Attr(attr_);
   Client_SYS.Add_To_Attr('ONHOLD_REMARKS', onhold_remarks_, attr_); 
   
   C_Modify(temp_supplier_id_, attr_, 'DO');
   
   Get_Id_Version_By_Keys___( objid_, objversion_, temp_supplier_id_ );
   Set_Hold__(info_, objid_, objversion_, attr_, 'DO');
END Set_Hold;

FUNCTION Check_Mandatory_Fields(
   temp_supplier_id_ IN VARCHAR2) RETURN VARCHAR2
IS 
   output_                  VARCHAR2(32000);
   country_specific_        VARCHAR2(32000);
   ofc_address_contact_     VARCHAR2(32000);
   work_address_contact_    VARCHAR2(32000);
   org_info_                VARCHAR2(32000);
   financial_               VARCHAR2(32000);
   imm_                     VARCHAR2(32000);
   works_                   VARCHAR2(32000);
   outsource_               VARCHAR2(32000);
   count_mobile_            NUMBER;
   count_tel_               NUMBER;
   count_email_             NUMBER;
   temp_supp_rec_           c_supplier_onboarding_tab%ROWTYPE;
   
   CURSOR get_sup_contacts(related_to_ IN VARCHAR2) IS
      SELECT COUNT(telephone_no), 
             COUNT(mobile_no), 
             COUNT(e_mail)
      FROM   c_supplier_onboard_comm_tab
      WHERE  temp_supplier_id = temp_supplier_id_
      AND    related_to       = related_to_;
   
   CURSOR get_imm_details IS
      SELECT rowid
      FROM   c_supplier_onboard_imm_TAB
      WHERE  temp_supplier_id = temp_supplier_id_;
   
   rec_imm_details_    get_imm_details%ROWTYPE;
   
   CURSOR get_hr_details IS
      SELECT rowid
      FROM   c_supplier_onboard_hr_TAB
      WHERE  temp_supplier_id = temp_supplier_id_;
   
   rec_hr_details_    get_hr_details%ROWTYPE;
   
   CURSOR get_works_details IS
      SELECT rowid,
             any_hal_subcontracts,
             subcontract_details,
             e_proc_portal_reg,
             is_e_proc_registered,
             epf_registration_no,
             is_esic_registered,
             esic_registration_no,
             is_ecl_avail,
             ecl_no,
             is_ccl_avail,
             ccl_no
      FROM   c_supplier_onboard_works_tab
      WHERE  temp_supplier_id = temp_supplier_id_;
   
   rec_works_details_    get_works_details%ROWTYPE;
   
   CURSOR get_outsource_details IS
      SELECT ROWID, 
             t.*
      FROM   c_supplier_onboard_subcon_tab t
      WHERE  temp_supplier_id = temp_supplier_id_;
   
   rec_outsource_details_    get_outsource_details%ROWTYPE;
   
   PROCEDURE Check_Null (
      output_       IN OUT VARCHAR2,
      field_label_  IN     VARCHAR2,
      value_        IN     VARCHAR2,
      based_on_     IN     VARCHAR2 DEFAULT NULL,
      based_on_val_ IN     VARCHAR2 DEFAULT NULL)
   IS
   BEGIN
      IF (value_ IS NULL) THEN
         IF based_on_ IS NULL OR (based_on_val_ IS NULL AND based_on_ = 'Y') OR (based_on_ = based_on_val_)THEN
            output_ := output_ || field_label_ || ', ';
         END IF;
      END IF;
   END Check_Null;
BEGIN 
   temp_supp_rec_ := Get_Object_By_Keys___(temp_supplier_id_); 
   
   IF temp_supp_rec_.country_origin = 'I' THEN
      Check_Null(country_specific_, 'Pan No', temp_supp_rec_.pan_no, temp_supp_rec_.pan_not_available,    'N');
      Check_Null(country_specific_, 'Gem Id', temp_supp_rec_.gem_id, temp_supp_rec_.gem_id_not_available, 'N');
   ELSE  
      Check_Null(country_specific_, 'Country Code', temp_supp_rec_.country_code);
      Check_Null(country_specific_, 'Country Code', temp_supp_rec_.duns_number,  temp_supp_rec_.duns_number_not_available, 'N');
   END IF;
   
   country_specific_ := RTRIM(RTRIM(country_specific_, ' '), ',');
   
   IF country_specific_ IS NOT NULL THEN
      output_ := output_ || chr(13)||chr(10) || 'Tab: Country Specific Information' || chr(13)||chr(10);
      output_ := output_ || chr(13)||chr(10) || '* ' || country_specific_;
   END IF;
   
   Check_Null(ofc_address_contact_, 'Address 1', temp_supp_rec_.Office_Address1);
   Check_Null(ofc_address_contact_, 'Address 2', temp_supp_rec_.Office_Address2);
   Check_Null(ofc_address_contact_, 'Zip Code',  temp_supp_rec_.Office_Zip_Code);
   Check_Null(ofc_address_contact_, 'City',      temp_supp_rec_.Office_City);
   Check_Null(ofc_address_contact_, 'Country',   temp_supp_rec_.Office_Country);
   Check_Null(ofc_address_contact_, 'State',     temp_supp_rec_.Office_State);
   
   ofc_address_contact_ := RTRIM(RTRIM(ofc_address_contact_, ' '), ',');
   
   OPEN  get_sup_contacts('OFFICE');
   FETCH get_sup_contacts INTO count_tel_, count_mobile_, count_email_;
   CLOSE get_sup_contacts;
   
   IF count_tel_ = 0 AND count_mobile_ = 0 AND count_email_ = 0 THEN
      IF ofc_address_contact_ IS NOT NULL THEN
         ofc_address_contact_ := ofc_address_contact_ || chr(13)||chr(10) || '* At least one contact information is needed with Telephone No or Mobile No or E-mail.' || chr(13)||chr(10);
      ELSE
         ofc_address_contact_ := ofc_address_contact_ || chr(13)||chr(10) || 'At least one contact information is needed with Telephone No or Mobile No or E-mail.' || chr(13)||chr(10);
      END IF;
   END IF;
   
   Check_Null(work_address_contact_, 'Address 1', temp_supp_rec_.work_Address1);
   Check_Null(work_address_contact_, 'Address 2', temp_supp_rec_.work_Address2);
   Check_Null(work_address_contact_, 'Zip Code',  temp_supp_rec_.work_Zip_Code);
   Check_Null(work_address_contact_, 'City',      temp_supp_rec_.work_City);
   Check_Null(work_address_contact_, 'Country',   temp_supp_rec_.work_Country);
   Check_Null(work_address_contact_, 'State',     temp_supp_rec_.work_State);
   
   work_address_contact_ := RTRIM(RTRIM(work_address_contact_, ' '), ',');
   
   OPEN  get_sup_contacts('WORK');
   FETCH get_sup_contacts INTO count_tel_, count_mobile_, count_email_;
   CLOSE get_sup_contacts;
   
   IF count_tel_ = 0 AND count_mobile_ = 0 AND count_email_ = 0 THEN
      IF work_address_contact_ IS NOT NULL THEN
         work_address_contact_ := work_address_contact_ || chr(13)||chr(10) || '* At least one contact information is needed with Telephone No or Mobile No or E-mail.' || chr(13)||chr(10);
      ELSE
         work_address_contact_ := work_address_contact_ || chr(13)||chr(10) || 'At least one contact information is needed with Telephone No or Mobile No or E-mail.' || chr(13)||chr(10);
      END IF;
   END IF;
   
   IF ofc_address_contact_ IS NOT NULL OR work_address_contact_ IS NOT NULL THEN
      output_ := output_ || chr(13)||chr(10) || chr(13)||chr(10) || 'Tab: Addresses & Contacts' || chr(13)||chr(10);
   END IF;
   
   IF ofc_address_contact_ IS NOT NULL THEN
      output_ := output_ || chr(13)||chr(10) || '-- Office Address:' || chr(13)||chr(10);
      output_ := output_ || chr(13)||chr(10) || '* ' || ofc_address_contact_;
   END IF;
   
   IF work_address_contact_ IS NOT NULL THEN
      output_ := output_ || chr(13)||chr(10) || '-- Work Facility Address:' || chr(13)||chr(10);
      output_ := output_ || chr(13)||chr(10) || '* ' || work_address_contact_;
   END IF;
   
   Check_Null(org_info_, 'Nature of Business',               temp_supp_rec_.nature_of_business);
   Check_Null(org_info_, 'Year of commencement of Business', temp_supp_rec_.year_of_commencement);
   Check_Null(org_info_, 'GSTIN',                            temp_supp_rec_.gstin_no,                temp_supp_rec_.is_gstin_applicable, 'APPLICABLE');
   Check_Null(org_info_, 'Udyog Aadhar No /MSME No',         temp_supp_rec_.udyog_aadhar_or_msme_no, temp_supp_rec_.udyog_aadhar);
   Check_Null(org_info_, 'MseOwnership',                     temp_supp_rec_.mse_ownership);
   Check_Null(org_info_, 'Govt Agency Approved',             temp_supp_rec_.govt_agency,             temp_supp_rec_.appr_by_govt_agency);
   
   Check_Null(org_info_, 'Registration Particulars for Service Providers', temp_supp_rec_.reg_particulars);
   Check_Null(org_info_, 'Litigation / Arbitration Cases Details',         temp_supp_rec_.case_details,      temp_supp_rec_.is_under_cases);
   Check_Null(org_info_, 'Delisted / debarred Details',                    temp_supp_rec_.exclusion_details, temp_supp_rec_.is_under_exclusion);
   
   Check_Null(org_info_, 'List of your major Customers with contact address',                           temp_supp_rec_.major_customers_details);
   Check_Null(org_info_, 'List of the names & address of all associates, subsidiary & Holding company', temp_supp_rec_.associate_details);
   Check_Null(org_info_, 'Banning / Suspension details (Levels Division / Complex / Company)',          temp_supp_rec_.ban_details);
      
   org_info_ := RTRIM(RTRIM(org_info_, ' '), ',');
   
   IF org_info_ IS NOT NULL THEN
      output_ := output_ || chr(13) || chr(10) || chr(13) || chr(10) || 'Tab: Organization Information' || chr(13)||chr(10);
      output_ := output_ || chr(13)||chr(10) || '* ' || org_info_;
   END IF;
   
   IF temp_supp_rec_.hal_division IS NULL THEN
      output_ := output_ || chr(13) || chr(10) || chr(13) || chr(10) || 'Tab: Division Registration' || chr(13) || chr(10);
      output_ := output_ || '* Primary Hal Division' ;
   END IF;
   
   Check_Null(financial_, 'Annual Turnover Past 1 Year',        temp_supp_rec_.ANN_TURNOVER_PAST_YR1);
   Check_Null(financial_, 'Annual Turnover Past 2 Year',        temp_supp_rec_.ANN_TURNOVER_PAST_YR2);
   Check_Null(financial_, 'Annual Turnover Past 3 Year',        temp_supp_rec_.ANN_TURNOVER_PAST_YR3);
   Check_Null(financial_, 'Profit/Loss Past 1 Year',            temp_supp_rec_.PROFIT_OR_LOSS_PAST_YR1);
   Check_Null(financial_, 'Profit/Loss Past 2 Year',            temp_supp_rec_.PROFIT_OR_LOSS_PAST_YR2);
   Check_Null(financial_, 'Profit/Loss Past 3 Year',            temp_supp_rec_.PROFIT_OR_LOSS_PAST_YR3);
   Check_Null(financial_, 'Name of the Bank',                   temp_supp_rec_.bank_name);
   Check_Null(financial_, 'Name of Bank Branch',                temp_supp_rec_.bank_branch_name);
   Check_Null(financial_, 'City/Place',                         temp_supp_rec_.bank_city);
   Check_Null(financial_, 'Account Number/IBAN Number',         temp_supp_rec_.bank_account_no);
   Check_Null(financial_, 'Account Type',                       temp_supp_rec_.bank_account_type);
   Check_Null(financial_, 'IFSC code of the Bank Branch/SWIFT', temp_supp_rec_.bank_ifsc_code);
   
   financial_ := RTRIM(RTRIM(financial_, ' '), ',');
   
   BEGIN
      Check_Bank_Account_No(temp_supp_rec_.bank_account_no, temp_supp_rec_.supplier_email, temp_supp_rec_.pan_no, temp_supp_rec_.duns_number);
   EXCEPTION
      WHEN OTHERS THEN
         financial_ := financial_ || chr(13)||chr(10) || '* ' || SUBSTR(SQLERRM, INSTR(SQLERRM, ':', -1) + 2);
   END;
   
   IF financial_ IS NOT NULL THEN
      output_ := output_ || chr(13)||chr(10) || chr(13) || chr(10) || 'Tab: Financial Soundness' || chr(13) || chr(10);
      output_ := output_ || chr(13)||chr(10) || '* ' || financial_;
   END IF;
   
   IF temp_supp_rec_.registration_for = 'IMM' THEN
      OPEN  get_imm_details;
      FETCH get_imm_details INTO rec_imm_details_;
      CLOSE get_imm_details;
      
      IF rec_imm_details_.rowid IS NULL THEN       
         output_ := output_ || chr(13) || chr(10) || chr(13)||chr(10) || 'Tab: IMM' || chr(13) || chr(10);
         output_ := output_ || chr(13) || chr(10) ||  '* Create IMM record with mandatory data' || chr(13) || chr(10);
      END IF;
   END IF;
   
   output_ := RTRIM(RTRIM(output_, ' '), ',');
   
   IF temp_supp_rec_.registration_for = 'HR' THEN
      OPEN  get_hr_details;
      FETCH get_hr_details INTO rec_hr_details_;
      CLOSE get_hr_details;
      
      IF rec_hr_details_.rowid IS NULL THEN       
         output_ := output_ || chr(13) || chr(10) || chr(13)||chr(10) || 'Tab: HR' || chr(13) || chr(10);
         output_ := output_ || chr(13) || chr(10) ||  '* Create HR record with mandatory data' || chr(13) || chr(10);
      END IF;
   END IF;
   
   output_ := RTRIM(RTRIM(output_, ' '), ',');
   
   IF temp_supp_rec_.registration_for = 'WORKS' THEN
      OPEN  get_works_details;
      FETCH get_works_details INTO rec_works_details_;
      CLOSE get_works_details;
      
      IF rec_works_details_.rowid IS NULL THEN       
         works_ := chr(13)||chr(10) || chr(13)||chr(10) || 'Create Works record with mandatory data' || chr(13)||chr(10);
      ELSE
         Check_Null(works_, 'Subcontract Work Details',      rec_works_details_.subcontract_details,  rec_works_details_.any_hal_subcontracts);
         Check_Null(works_, 'EPF Registration No',           rec_works_details_.epf_registration_no,  rec_works_details_.is_e_proc_registered);
         Check_Null(works_, 'ESIC Registration No',          rec_works_details_.esic_registration_no, rec_works_details_.is_esic_registered);
         Check_Null(works_, 'Electrical Contractor License', rec_works_details_.ecl_no,               rec_works_details_.is_ecl_avail);
         Check_Null(works_, 'Civil Contractor License',      rec_works_details_.ccl_no,               rec_works_details_.is_ccl_avail);
      END IF;
   END IF;
   
   works_ := RTRIM(RTRIM(works_, ' '), ',');
   
   IF works_ IS NOT NULL THEN
      output_ := output_ || chr(13)||chr(10) || chr(13)||chr(10) || 'Tab: WORKS' || chr(13)||chr(10);
      output_ := output_ || chr(13)||chr(10) || '* ' || works_;
   END IF;
   
   IF temp_supp_rec_.registration_for = 'OUTSOURCE' THEN
      OPEN  get_outsource_details;
      FETCH get_outsource_details INTO rec_outsource_details_;
      CLOSE get_outsource_details;
      
      IF rec_outsource_details_.rowid IS NULL THEN       
         outsource_ := chr(13)||chr(10) ||  'Create Outsource record with mandatory data' || chr(13)||chr(10);
      ELSE
         Check_Null(outsource_, 'Work on subcontract from HAL Details', rec_outsource_details_.past_hal_subcontract_det, rec_outsource_details_.any_past_hal_subcontract);
         Check_Null(outsource_, 'Work for customers other than HAL Details', rec_outsource_details_.past_other_subcon_det, rec_outsource_details_.any_past_other_subcontract);
         Check_Null(outsource_, 'Bankrupt or compounded with or made an assignment for the benefit of creditors Details', rec_outsource_details_.bankrupt_details, rec_outsource_details_.is_bankrupt);
         Check_Null(outsource_, 'Any company which has taken or had instigated against it any action Details', rec_outsource_details_.instigate_details, rec_outsource_details_.any_instigate_actions);
         Check_Null(outsource_, 'Qualifications and experience of key Technical personnel', rec_outsource_details_.key_tech_qualifications);
         Check_Null(outsource_, 'Company''s Total Technical Personnel by Discipline', rec_outsource_details_.key_tech_by_discipline);
         Check_Null(outsource_, 'Give details of major equipment of Plant, Machinery & Inspection Equipment''s available', rec_outsource_details_.plant_machinery_details);
         Check_Null(outsource_, 'Give the floor area of your factory and Offices. If you own more than one factory, please give separate details for each unit', rec_outsource_details_.floor_area_details);
         Check_Null(outsource_, 'Indicate whether separate storage facilities are / can be made available for storage of aircraft materials to avoid any mix up with commercial materials', rec_outsource_details_.storage_facilities_details);
         Check_Null(outsource_, 'List total value of Sub-Contract work performed in the last three years', rec_outsource_details_.past_total_work_value);
         Check_Null(outsource_, 'Please provide any additional information, which will help you in securing registration with HAL', rec_outsource_details_.additional_information);
      END IF;
   END IF;
   
   outsource_ := RTRIM(RTRIM(outsource_, ' '), ',');
   
   IF outsource_ IS NOT NULL THEN 
      output_ := output_ || chr(13)||chr(10) || chr(13)||chr(10) || 'Tab: OUTSOURCE' || chr(13) || chr(10);
      output_ := output_ || chr(13)||chr(10) || '* ' || outsource_;
   END IF;
   
   RETURN NVL(trim(output_), 'OK');
END Check_Mandatory_Fields;
--(+) 220921 TecPardhY BLK-IMM-15-M (FINISH)

--(+) 220921 TecRahulM BLK-IMM-15-M (START)
PROCEDURE Validate_In_Pincode(
   country_code_ IN VARCHAR2,
   pincode_ IN VARCHAR2) 
IS
  pincode_length_ NUMBER := 0;
BEGIN
   pincode_length_ := length(pincode_);
   IF country_code_ ='IN' AND pincode_length_ <> 6
   THEN
      Error_SYS.Record_General(lu_name_,'Indian Pincode should be of length 6');
   END IF;
END;
--(+) 220921 TecRahulM BLK-IMM-15-M (FINISH)

FUNCTION Validate_Pan_No (
   pan_no_ IN VARCHAR2) RETURN BOOLEAN
IS
BEGIN
   IF REGEXP_LIKE(pan_no_, '^[A-Z]{5}[0-9]{4}[A-Z]{1}$') THEN
      RETURN TRUE;
   END IF;
   
   RETURN FALSE;
END Validate_Pan_No;

PROCEDURE C_Multi_Supplier_Updates(
   supplier_id_      IN     VARCHAR2,
   attr_             IN     VARCHAR2)
IS
   attr1_                   VARCHAR2(32000);
   
   CURSOR get_onboardings IS
      SELECT temp_supplier_id
      FROM   c_supplier_onboarding_tab 
      WHERE  ifs_supplier_id = supplier_id_;
BEGIN
   FOR rec_ IN get_onboardings LOOP
      attr1_ := attr_;
      C_Modify(rec_.temp_supplier_id, attr1_, 'DO');
   END LOOP;
END C_Multi_Supplier_Updates;

PROCEDURE C_New(
   attr_       IN OUT VARCHAR2,
   action_     IN     VARCHAR2 )
IS
   objid_              VARCHAR2(100);
   objversion_         VARCHAR2(100);
   
   newrec_            c_supplier_onboarding_tab%ROWTYPE;
   indrec_            Indicator_Rec;
BEGIN
   IF (action_ = 'PREPARE') THEN
      Prepare_Insert___(attr_);
   ELSIF (action_ = 'CHECK') THEN
      Unpack___(newrec_, indrec_, attr_);
      Check_Insert___(newrec_, indrec_, attr_);
   ELSIF (action_ = 'DO') THEN
      Unpack___(newrec_, indrec_, attr_);
      Check_Insert___(newrec_, indrec_, attr_);
      Insert___(objid_, objversion_, newrec_, attr_);
   END IF;
END C_New;

PROCEDURE C_Modify(
   temp_supplier_id_ IN     VARCHAR2,
   attr_             IN OUT VARCHAR2,
   action_           IN     VARCHAR2)
IS
   objid_              VARCHAR2(100);
   objversion_         VARCHAR2(100);
   
   oldrec_             c_supplier_onboarding_tab%ROWTYPE;
   newrec_             c_supplier_onboarding_tab%ROWTYPE;
   indrec_             Indicator_Rec;
BEGIN
   IF (action_ = 'CHECK') THEN
      oldrec_ := Get_Object_By_Keys___(temp_supplier_id_);
      newrec_ := oldrec_;
      Unpack___(newrec_, indrec_, attr_);
      Check_Update___(oldrec_, newrec_, indrec_, attr_);
   ELSIF (action_ = 'DO') THEN
      oldrec_ := Lock_By_Keys_Nowait___(temp_supplier_id_);
      newrec_ := oldrec_;
      Unpack___(newrec_, indrec_, attr_);
      Check_Update___(oldrec_, newrec_, indrec_, attr_);
      Update___(objid_, oldrec_, newrec_, attr_, objversion_, TRUE);
   END IF;
END C_Modify;

PROCEDURE Check_Bank_Account_No(
   bank_account_no_ IN VARCHAR2,
   email_id_        IN VARCHAR2,
   pan_no_          IN VARCHAR2,
   duns_number_     IN VARCHAR2)
IS
   acc_supplier_id_  c_supplier_bank_details_tab.supplier_Id%TYPE;
   sob_supplier_id_  c_supplier_bank_details_tab.supplier_Id%TYPE;
   
   CURSOR get_supplier IS
      SELECT ifs_supplier_id 
      FROM   c_supplier_onboarding_tab t
      WHERE  t.supplier_email = email_id_
      AND    ((pan_no_ IS NULL AND pan_no IS NULL) OR pan_no = pan_no_) 
      AND    ((duns_number_ IS NULL AND duns_number IS NULL) OR duns_number = duns_number_)
      AND    t.rowstate       = 'Created';
      
   CURSOR check_account_supp IS
      SELECT supplier_id 
      FROM   c_supplier_bank_details_tab
      WHERE  bank_account_no = bank_account_no_;
BEGIN
	OPEN  check_account_supp;
   FETCH check_account_supp INTO acc_supplier_id_;
   CLOSE check_account_supp;

   IF acc_supplier_id_ IS NOT NULL THEN
      OPEN  get_supplier;
      FETCH get_supplier INTO sob_supplier_id_;
      CLOSE get_supplier;
      
      IF sob_supplier_id_ IS NOT NULL THEN
         IF acc_supplier_id_ != sob_supplier_id_ THEN
            Error_Sys.Record_General(lu_name_, 'ERRBANODUP: Bank Account No already used in the supplier :P1.', acc_supplier_id_);
         END IF;
      ELSE
         Error_Sys.Record_General(lu_name_, 'ERRBANODUP: Bank Account No already used in the supplier :P1.', acc_supplier_id_);
      END IF;
   END IF;
END Check_Bank_Account_No;
