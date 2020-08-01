CLASS zcl_ab_todo_res_controller DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS co_class_name TYPE seoclsname VALUE 'ZCL_AB_TODO_RES_CONTROLLER'. "#EC NOTEXT
    CONSTANTS co_resource_type TYPE string VALUE 'ZAB_TODO_TT'. "#EC NOTEXT
    CONSTANTS co_st_name TYPE string VALUE 'ZAB_TODO_SIMPLE_TRANS'. "#EC NOTEXT
    CONSTANTS co_root_name TYPE string VALUE 'TODO_DATA'.   "#EC NOTEXT

    TYPES: BEGIN OF attributes_ts,
             obj_name         TYPE string,
             scan_todo        TYPE abap_bool,
             scan_fixme       TYPE abap_bool,
             scan_xxx         TYPE abap_bool,
             custom_text      TYPE string,
             deep_scan        TYPE abap_bool,
             created_by_me    TYPE abap_bool,
             scan_source_code TYPE abap_bool,
           END OF attributes_ts.

    CLASS-METHODS get_content_handler RETURNING VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    METHODS get REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_key_from_uri IMPORTING request       TYPE REF TO if_adt_rest_request
                             RETURNING VALUE(result) TYPE attributes_ts
                             RAISING   cx_adt_rest.
ENDCLASS.



CLASS zcl_ab_todo_res_controller IMPLEMENTATION.

  METHOD get.

    DATA(attributes) = get_key_from_uri( request = request ).

    DATA(todos) = zcl_ab_code_scanner=>get_instance(  )->scan( attributes ).

    response->set_body_data( content_handler = get_content_handler(  )
                             data            = todos ).

  ENDMETHOD.

  METHOD get_content_handler.

    result = cl_adt_rest_cnt_hdl_factory=>get_instance(  )->get_handler_for_xml_using_st(
                                                         st_name           = co_st_name
                                                         root_name         = co_root_name ).
  ENDMETHOD.

  METHOD get_key_from_uri.
    CONSTANTS type_abap_bool TYPE string VALUE 'ABAP_BOOL'.
    CONSTANTS java_true TYPE string VALUE 'true'.
    CONSTANTS java_false TYPE string VALUE 'false'.

    DATA(attributes) = VALUE attributes_ts(  ).

    DATA(description) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( attributes ) ).
    DATA(components) = description->get_components( ).

    DATA(attr_value) = ``.
    LOOP AT components INTO DATA(component).

      ASSIGN COMPONENT component-name OF STRUCTURE attributes TO FIELD-SYMBOL(<value>).

      request->get_uri_attribute(
        EXPORTING
          name        = component-name
          mandatory   = abap_true
        IMPORTING
          value       = attr_value
      ).

      CONDENSE attr_value.

      IF component-type->get_relative_name(  ) = type_abap_bool.

        <value> = SWITCH abap_bool( attr_value
                                    WHEN java_true THEN abap_true
                                    WHEN java_false THEN abap_false ).
        CONTINUE.
      ENDIF.

      <value> = attr_value.
    ENDLOOP.

    result = attributes.
  ENDMETHOD.

ENDCLASS.
