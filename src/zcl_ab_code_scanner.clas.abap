CLASS zcl_ab_code_scanner DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    METHODS scan IMPORTING attributes    TYPE zcl_ab_todo_res_controller=>attributes_ts
                 RETURNING VALUE(result) TYPE zab_todo_tt.

    CLASS-METHODS get_instance RETURNING VALUE(result) TYPE REF TO zcl_ab_code_scanner.

    CLASS-METHODS class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF package_object,
             name         TYPE sobj_name,
             type         TYPE trobjtype,
             parent       TYPE sobj_name,
             include_name TYPE sobj_name,
           END OF package_object.
    TYPES package_objects TYPE SORTED TABLE OF package_object WITH NON-UNIQUE KEY type name.

    TYPES:
      BEGIN OF relevant_todo,
        relevant TYPE abap_bool.
        INCLUDE  TYPE zab_todo.
    TYPES END OF relevant_todo.
    TYPES relevant_todos TYPE SORTED TABLE OF relevant_todo WITH NON-UNIQUE KEY relevant.

    TYPES: BEGIN OF relevant_todo_info,
             todo_description TYPE string,
             is_relevant      TYPE abap_bool,
           END OF relevant_todo_info.

    CONSTANTS todo TYPE string VALUE 'TODO'.
    CONSTANTS fixme TYPE string VALUE 'FIXME'.
    CONSTANTS xxx TYPE string VALUE 'XXX'.

    CONSTANTS type_not_found_error TYPE string VALUE 'TYPE_NOT_FOUND'.

    CONSTANTS type_package TYPE trobjtype VALUE 'DEVC'.
    CONSTANTS type_class TYPE trobjtype VALUE 'CLAS'.
    CONSTANTS type_interface TYPE trobjtype VALUE 'INTF'.
    CONSTANTS type_program TYPE trobjtype VALUE 'PROG'.
    CONSTANTS type_function_group TYPE trobjtype VALUE 'FUGR'.
    CONSTANTS type_function_module TYPE trobjtype VALUE 'FM'.
    CONSTANTS type_include TYPE trobjtype VALUE 'INCL'.
    CONSTANTS type_testclass TYPE trobjtype VALUE 'TEST'.
    CONSTANTS type_localclass TYPE trobjtype VALUE 'LCL'.
    CONSTANTS type_transport_request TYPE trobjtype VALUE 'TR'.

    CONSTANTS local_package TYPE string VALUE '$TMP'.

    CLASS-DATA instance TYPE REF TO zcl_ab_code_scanner.

    METHODS scan_programs_includes_fms IMPORTING objects       TYPE package_objects
                                                 attributes    TYPE zcl_ab_todo_res_controller=>attributes_ts
                                       RETURNING VALUE(result) TYPE relevant_todos.

    METHODS scan_classes_interfaces IMPORTING objects       TYPE package_objects
                                              attributes    TYPE zcl_ab_todo_res_controller=>attributes_ts
                                    RETURNING VALUE(result) TYPE relevant_todos.

    METHODS scan_local_classes IMPORTING classes       TYPE package_objects
                                         attributes    TYPE zcl_ab_todo_res_controller=>attributes_ts
                               RETURNING VALUE(result) TYPE relevant_todos.

    METHODS get_object_type
      IMPORTING
        object_name   TYPE string
      RETURNING
        VALUE(result) TYPE tadir-object.

    METHODS get_objects_of_package IMPORTING packages      TYPE package_objects
                                             attributes    TYPE zcl_ab_todo_res_controller=>attributes_ts
                                   RETURNING VALUE(result) TYPE package_objects.

    METHODS get_objects_of_local_package IMPORTING author        TYPE string
                                         RETURNING VALUE(result) TYPE package_objects.

    METHODS get_function_modules IMPORTING function_groups TYPE package_objects
                                 RETURNING VALUE(result)   TYPE package_objects.

    METHODS get_relevant_object_types
      RETURNING
        VALUE(result) TYPE rsdsselopt_t.

    METHODS get_supported_object_types
      RETURNING
        VALUE(result) TYPE rsdsselopt_t.

    METHODS get_function_module_include
      IMPORTING function_module TYPE tfdir
      RETURNING VALUE(result)   TYPE string.

    METHODS get_todo_info IMPORTING pattern       TYPE string
                                    code_line     TYPE string
                          RETURNING VALUE(result) TYPE relevant_todo_info.

    METHODS get_includes IMPORTING programs_includes TYPE package_objects
                         RETURNING VALUE(result)     TYPE package_objects.

    METHODS get_subpackages_of_local_pack IMPORTING author        TYPE string
                                          RETURNING VALUE(result) TYPE package_objects.

    METHODS get_subpackages_of_packages IMPORTING packages      TYPE package_objects
                                        RETURNING VALUE(result) TYPE package_objects.

    METHODS is_relevant
      IMPORTING
        pattern       TYPE string
        code_line     TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS get_packages_to_scan IMPORTING object_name   TYPE string
                                           object_type   TYPE trobjtype
                                           attributes    TYPE zcl_ab_todo_res_controller=>attributes_ts
                                 RETURNING VALUE(result) TYPE package_objects.

    METHODS scan_for_patterns
      IMPORTING patterns      TYPE string_t
                object        TYPE zcl_ab_code_scanner=>package_object
                source_code   TYPE rswsourcet
      RETURNING VALUE(result) TYPE zcl_ab_code_scanner=>relevant_todos.

    METHODS get_patterns
      IMPORTING attributes    TYPE zcl_ab_todo_res_controller=>attributes_ts
      RETURNING VALUE(result) TYPE string_t.

    METHODS get_objects_to_scan_of_packs
      IMPORTING
        attributes       TYPE zcl_ab_todo_res_controller=>attributes_ts
        object_name      TYPE zcl_ab_todo_res_controller=>attributes_ts-obj_name
        object_type      TYPE trobjtype
        packages_to_scan TYPE zcl_ab_code_scanner=>package_objects
      RETURNING
        VALUE(result)    TYPE zcl_ab_code_scanner=>package_objects.

    METHODS check_if_function_module
      IMPORTING
        object_name   TYPE string
      RETURNING
        VALUE(result) TYPE trobjtype .

    METHODS check_if_transport_request
      IMPORTING
        object_name   TYPE string
      RETURNING
        VALUE(result) TYPE trobjtype .

    METHODS check_if_local_package
      IMPORTING object_name   TYPE string
      RETURNING VALUE(result) TYPE trobjtype .

    METHODS get_tasks_trs_to_scan
      IMPORTING object_name   TYPE string
      RETURNING VALUE(result) TYPE package_objects.

    METHODS get_objects_to_scan_of_trs
      IMPORTING tasks_trs_to_scan TYPE package_objects
                attributes        TYPE zcl_ab_todo_res_controller=>attributes_ts
      RETURNING VALUE(result)     TYPE package_objects.

    METHODS get_objects_to_scan
      IMPORTING
        attributes    TYPE zcl_ab_todo_res_controller=>attributes_ts
        object_name   TYPE zcl_ab_todo_res_controller=>attributes_ts-obj_name
        object_type   TYPE trobjtype
      RETURNING
        VALUE(result) TYPE zcl_ab_code_scanner=>package_objects.

ENDCLASS.



CLASS zcl_ab_code_scanner IMPLEMENTATION.

  METHOD scan.

    DATA(object_name) = attributes-obj_name.
    DATA(object_type) = get_object_type( object_name ).

    IF object_type IS INITIAL.

      result = VALUE #( ( type = type_not_found_error ) ).
      RETURN.
    ENDIF.

    DATA(objects_to_scan) = get_objects_to_scan( attributes  = attributes
                                                 object_name = object_name
                                                 object_type = object_type ).

    DATA(classes) = FILTER package_objects( objects_to_scan WHERE type = type_class ).
    DATA(interfaces) = FILTER package_objects( objects_to_scan WHERE type = type_interface ).
    DATA(programs_includes) = FILTER package_objects( objects_to_scan WHERE type = type_program ).
    DATA(function_groups) = FILTER package_objects( objects_to_scan WHERE type = type_function_group ).
    DATA(function_modules) = FILTER package_objects( objects_to_scan WHERE type = type_function_module ).

    DATA(includes) = get_includes( programs_includes ).
    DATA(programs) = programs_includes.

    DATA(includes_selopt) = VALUE rsdsselopt_t( FOR <include> IN includes
                                                  ( sign   = 'I'
                                                    option = 'EQ'
                                                    low    = <include>-name ) ).

    IF includes_selopt IS NOT INITIAL.

      DELETE programs WHERE name IN includes_selopt.
    ENDIF.

    programs_includes = CORRESPONDING #( ( programs ) includes ).

    function_modules = CORRESPONDING #( get_function_modules( CORRESPONDING #( ( function_groups ) function_modules ) ) ).

    DATA(all_todos) = CORRESPONDING zab_todo_tt( scan_classes_interfaces( objects    = CORRESPONDING #( ( classes ) interfaces )
                                                                          attributes = attributes ) ).
    all_todos = CORRESPONDING #( ( all_todos ) scan_programs_includes_fms( objects    = CORRESPONDING #( ( programs_includes ) function_modules )
                                                                           attributes = attributes ) ).
    all_todos = CORRESPONDING #( ( all_todos ) scan_local_classes( classes    = classes
                                                                   attributes = attributes ) ).
    result = all_todos.
  ENDMETHOD.


  METHOD scan_programs_includes_fms.

    DATA(source_code) = VALUE string_t(  ).
    DATA(todos) = VALUE match_result_tab(  ).
    DATA(all_todos) = VALUE relevant_todos(  ).
    DATA(patterns) = get_patterns( attributes ).

    LOOP AT objects ASSIGNING FIELD-SYMBOL(<object>).

      DATA(object_name) = SWITCH sobj_name( <object>-type
                                            WHEN type_function_module THEN <object>-include_name
                                            ELSE <object>-name ).

      READ REPORT object_name INTO source_code.

      all_todos = CORRESPONDING #( ( all_todos ) scan_for_patterns( object      = <object>
                                                                    source_code = source_code
                                                                    patterns    = patterns ) ).
    ENDLOOP.

    result = all_todos.
  ENDMETHOD.


  METHOD scan_classes_interfaces.
    DATA(all_todos) = VALUE relevant_todos(  ).

    DATA(patterns) = get_patterns( attributes ).

    LOOP AT objects ASSIGNING FIELD-SYMBOL(<object>).

      DATA(source_scanner) = cl_oo_factory=>create_instance( )->create_clif_source( <object>-name ).
      source_scanner->get_source( IMPORTING source = DATA(source_code) ).

      all_todos = CORRESPONDING #( ( all_todos ) scan_for_patterns( object      = <object>
                                                                    source_code = source_code
                                                                    patterns    = patterns ) ).
    ENDLOOP.

    result = all_todos.
  ENDMETHOD.


  METHOD get_instance.

    result = instance.
  ENDMETHOD.


  METHOD class_constructor.

    instance = NEW #(  ).
  ENDMETHOD.


  METHOD get_object_type.

    DATA(object_type) = VALUE trobjtype(  ).

    object_type = check_if_local_package( object_name ).
    IF object_type IS NOT INITIAL.

      result = object_type.
      RETURN.
    ENDIF.

    DATA(supported_types) = get_supported_object_types( ).

    SELECT SINGLE object FROM tadir INTO @object_type  WHERE obj_name = @object_name
                                                         AND object IN @supported_types
                                                         AND delflag = @abap_false.

    IF sy-subrc <> 0.

      object_type = check_if_function_module( object_name ).

      IF object_type IS INITIAL.

        object_type = check_if_transport_request( object_name ).
      ENDIF.
    ENDIF.

    result = object_type.
  ENDMETHOD.


  METHOD get_objects_of_package.

    IF packages IS INITIAL.

      RETURN.
    ENDIF.

    DATA(packages_selopt) = VALUE rsdsselopt_t( FOR <package> IN packages
                                                  ( sign   = 'I'
                                                    option = 'EQ'
                                                    low    = <package>-name ) ).

    DATA(object_types) = get_relevant_object_types( ).

    DATA(author) = VALUE rsdsselopt_t(  ).
    IF attributes-created_by_me = abap_true.

      author = VALUE #( ( sign   = 'I'
                          option = 'EQ'
                          low    = sy-uname ) ).
    ENDIF.

    SELECT obj_name, object FROM tadir INTO TABLE @DATA(objects) WHERE devclass IN @packages_selopt
                                                                   AND object IN @object_types
                                                                   AND delflag = @abap_false
                                                                   AND author IN @author.

    result = CORRESPONDING #( objects MAPPING name = obj_name type = object ).
  ENDMETHOD.


  METHOD get_function_modules.

    IF function_groups IS INITIAL.

      RETURN.
    ENDIF.

    DATA(function_groups_selopt) = VALUE rsdsselopt_t( FOR <function_group> IN function_groups
                                                           ( sign   = 'I'
                                                             option = 'EQ'
                                                             low    = <function_group>-name ) ).

    SELECT * FROM enlfdir INTO TABLE @DATA(function_modules) WHERE area     IN @function_groups_selopt
                                                                OR funcname IN @function_groups_selopt.

    DATA(function_modules_selopt) = VALUE rsdsselopt_t( FOR <function_module> IN function_modules
                                                           ( sign   = 'I'
                                                             option = 'EQ'
                                                             low    = <function_module>-funcname ) ).

    SELECT * FROM tfdir INTO TABLE @DATA(function_module_includes) WHERE funcname IN @function_modules_selopt.

    result = VALUE #( FOR <function_module_incl> IN function_module_includes
                        ( name   = <function_module_incl>-funcname
                          type   = type_function_module
                          parent = function_modules[ funcname = <function_module_incl>-funcname ]-area
                          include_name = get_function_module_include( <function_module_incl> ) ) ).
  ENDMETHOD.


  METHOD get_objects_of_local_package.

    DATA(object_types) = get_relevant_object_types(  ).

    SELECT obj_name, object FROM tadir INTO TABLE @DATA(objects) WHERE devclass = @local_package
                                                                   AND object  IN @object_types
                                                                   AND delflag  = @abap_false
                                                                   AND author   = @author.
    result = CORRESPONDING #( objects MAPPING name = obj_name type = object ).
  ENDMETHOD.


  METHOD get_relevant_object_types.

    result  = VALUE rsdsselopt_t( ( sign   = 'I'
                                    option = 'EQ'
                                    low    = type_class )
                                  ( sign   = 'I'
                                    option = 'EQ'
                                    low    = type_interface )
                                  ( sign   = 'I'
                                    option = 'EQ'
                                    low    = type_program )
                                  ( sign   = 'I'
                                    option = 'EQ'
                                    low    = type_function_group ) ).

  ENDMETHOD.


  METHOD get_function_module_include.

    result = substring_after( val = function_module-pname sub = 'SAP' ) && 'U' && function_module-include.
  ENDMETHOD.


  METHOD get_todo_info.

    DATA(todo_info) = VALUE relevant_todo_info( todo_description = substring_from( val = code_line sub = pattern )
                                                is_relevant      = is_relevant( code_line = code_line
                                                                                pattern   = pattern ) ).
    result = todo_info.
  ENDMETHOD.


  METHOD is_relevant.

    DATA(code) = code_line.
    CONDENSE code.

    result = abap_false.

    DATA(search_pattern) = '*"' && pattern && '*'.
    IF code CP search_pattern OR code+0(1) = '*'.

      result  = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_includes.

    IF programs_includes IS INITIAL.

      RETURN.
    ENDIF.

    DATA(includes_selopt) = VALUE rsdsselopt_t( FOR <programs_include> IN programs_includes
                                                      ( sign   = 'I'
                                                        option = 'EQ'
                                                        low    = <programs_include>-name ) ).

    SELECT * FROM wbcrossi INTO TABLE @DATA(includes) WHERE master  IN @includes_selopt
                                                        OR name IN @includes_selopt.

    result = VALUE #( FOR GROUPS <group> OF <include> IN includes
                        GROUP BY ( name = <include>-name )
                        ( name   = <group>-name
                          type   = type_include  ) ).
  ENDMETHOD.


  METHOD scan_local_classes.
    CONSTANTS testclass_suffix TYPE string VALUE '*=CCAU'.
    CONSTANTS localclass_suffix TYPE string VALUE '*=CCIMP'.
    DATA source_code TYPE TABLE OF string.

    IF classes IS INITIAL.

      RETURN.
    ENDIF.

    DATA(class_selopt) = VALUE rsdsselopt_t( FOR <class> IN classes
                                               ( sign   = 'I'
                                                 option = 'CP'
                                                 low    = <class>-name && testclass_suffix )
                                               ( sign   = 'I'
                                                 option = 'CP'
                                                 low    = <class>-name && localclass_suffix ) ).

    DATA(local_classes) = VALUE package_objects(  ).

    SELECT * FROM trdir INTO CORRESPONDING FIELDS OF TABLE local_classes WHERE name IN class_selopt.

    DATA(all_todos) = VALUE relevant_todos(  ).

    DATA(patterns) = get_patterns( attributes ).

    LOOP AT local_classes INTO DATA(local_class).

      READ REPORT local_class-name INTO source_code.

      DATA(todos) = scan_for_patterns( object      = local_class
                                       source_code = source_code
                                       patterns    = patterns ).

      DATA(class_name) = substring_before( val = local_class-name sub = '=' ).

      DATA(class_type) = type_testclass.
      IF local_class-name CP localclass_suffix.

        class_type = type_localclass.
      ENDIF.

      all_todos = VALUE #( BASE all_todos
                             FOR <todo> IN todos
                               ( VALUE #( BASE CORRESPONDING #( <todo> ) obj_name    = class_name
                                                                         object_type = class_type ) ) ).
    ENDLOOP.

    result = all_todos.
  ENDMETHOD.


  METHOD get_subpackages_of_local_pack.

    SELECT * FROM tdevc INTO TABLE @DATA(subpacks) WHERE as4user  = @author
                                                     AND parentcl = ''.

    result = VALUE #( FOR <subpackage> IN subpacks
                        ( name = <subpackage>-devclass
                          type = type_package ) ).
  ENDMETHOD.

  METHOD get_subpackages_of_packages.

    DATA(all_subpackages) = VALUE package_objects(  ).
    LOOP AT packages INTO DATA(package).

      cl_pak_package_queries=>get_all_subpackages( EXPORTING
                                                     im_package = CONV #( package-name )
                                                     im_also_local_packages = abap_true
                                                   IMPORTING
                                                     et_subpackages = DATA(subpackages) ).

      all_subpackages = CORRESPONDING #( ( all_subpackages ) subpackages MAPPING name = package ).
    ENDLOOP.

    result = all_subpackages.
  ENDMETHOD.


  METHOD get_packages_to_scan.

    DATA(packages_to_scan) = VALUE package_objects(  ).

    IF object_type = type_package.

      packages_to_scan = VALUE #( BASE packages_to_scan ( name = object_name ) ).
    ENDIF.

    IF attributes-deep_scan = abap_true.

      IF object_type = local_package.

        packages_to_scan = get_subpackages_of_local_pack( object_name ).
      ENDIF.

      packages_to_scan = CORRESPONDING #( ( packages_to_scan ) get_subpackages_of_packages( packages_to_scan ) ).
    ENDIF.

    result = packages_to_scan.
  ENDMETHOD.


  METHOD scan_for_patterns.

    DATA(matches) = VALUE match_result_tab(  ).
    DATA(all_todos) = VALUE relevant_todos(  ).

    LOOP AT patterns INTO DATA(pattern).

      FIND ALL OCCURRENCES OF pattern IN TABLE source_code RESULTS matches.

      all_todos = VALUE #( BASE all_todos
                             FOR <todo> IN matches
                               LET relevant_todo_info = get_todo_info( code_line = source_code[ <todo>-line ]
                                                                       pattern   = pattern ) IN
                                ( description = relevant_todo_info-todo_description
                                  obj_name    = object-name
                                  object_type = object-type
                                  type        = pattern
                                  line        = <todo>-line
                                  parent      = object-parent
                                  relevant    = relevant_todo_info-is_relevant ) ).
    ENDLOOP.

    result = FILTER #( all_todos WHERE relevant = abap_true ).
  ENDMETHOD.


  METHOD get_patterns.

    DATA(patterns) = VALUE string_t( ).

    IF attributes-scan_todo = abap_true.

      patterns = VALUE #( BASE patterns ( todo ) ).
    ENDIF.

    IF attributes-scan_fixme = abap_true.

      patterns = VALUE #( BASE patterns ( fixme ) ).
    ENDIF.

    IF attributes-scan_xxx = abap_true.

      patterns = VALUE #( BASE patterns ( xxx ) ).
    ENDIF.

    IF attributes-custom_text IS NOT INITIAL.

      patterns = VALUE #( BASE patterns ( attributes-custom_text ) ).
    ENDIF.

    result = patterns.

  ENDMETHOD.


  METHOD get_objects_to_scan_of_packs.

    result  = VALUE package_objects( ( name = object_name
                                       type = object_type ) ).
    IF object_type = local_package.

      result = CORRESPONDING #( ( result ) get_objects_of_local_package( object_name ) ).
    ENDIF.

    result = CORRESPONDING #( ( result ) get_objects_of_package( packages   = packages_to_scan
                                                                 attributes = attributes ) ).

  ENDMETHOD.


  METHOD check_if_function_module.

    SELECT COUNT( * ) FROM enlfdir UP TO 1 ROWS WHERE funcname = @object_name.
    IF sy-subrc = 0.

      result = type_function_module.
    ENDIF.

  ENDMETHOD.

  METHOD check_if_transport_request.

    SELECT COUNT( * ) FROM e070 UP TO 1 ROWS WHERE trkorr = @object_name.
    IF sy-subrc = 0.

      result = type_transport_request.
    ENDIF.

  ENDMETHOD.

  METHOD get_objects_to_scan_of_trs.

    DATA(tr_selopt) = VALUE rsdsselopt_t( FOR <task> IN tasks_trs_to_scan
                                              ( sign   = 'I'
                                                option = 'EQ'
                                                low    = <task>-name ) ).

    DATA(object_types) = get_relevant_object_types( ).

    DATA(author) = VALUE rsdsselopt_t(  ).
    IF attributes-created_by_me = abap_true.

      author = VALUE #( ( sign   = 'I'
                          option = 'EQ'
                          low    = sy-uname ) ).
    ENDIF.

    SELECT trobj~* FROM e071 AS trobj
    INNER JOIN tadir ON trobj~obj_name = tadir~obj_name
    INTO TABLE @DATA(objects)
    WHERE trobj~trkorr IN @tr_selopt
      AND tadir~object IN @object_types
      AND tadir~delflag = @abap_false
      AND tadir~author IN @author.

    result = VALUE #( FOR GROUPS <group> OF <object> IN objects
                        GROUP BY ( name = <object>-obj_name
                                   type = <object>-object )
                          ( name = <group>-name
                            type = <group>-type ) ).
  ENDMETHOD.

  METHOD get_tasks_trs_to_scan.

    DATA(tasks_trs) = VALUE package_objects( ( name = object_name ) ).

    SELECT trkorr FROM e070 INTO TABLE @DATA(tasks) WHERE strkorr = @object_name.

    tasks_trs = VALUE #( BASE tasks_trs
                           FOR <task> IN tasks
                            ( name = <task> ) ).

    result = tasks_trs.
  ENDMETHOD.


  METHOD get_objects_to_scan.

    IF object_type = type_transport_request.

      DATA(tasks_trs_to_scan) = get_tasks_trs_to_scan( object_name ).
      result = get_objects_to_scan_of_trs( tasks_trs_to_scan = tasks_trs_to_scan
                                           attributes        = attributes ).
      RETURN.
    ENDIF.

    DATA(packages_to_scan) = get_packages_to_scan( object_name = object_name
                                                   object_type = object_type
                                                   attributes  = attributes ).
    result = get_objects_to_scan_of_packs( attributes       = attributes
                                           object_name      = object_name
                                           object_type      = object_type
                                           packages_to_scan = packages_to_scan ).
  ENDMETHOD.


  METHOD check_if_local_package.

    SELECT COUNT( * ) FROM usr21 UP TO 1 ROWS WHERE bname = @object_name.
    IF sy-subrc = 0.

      result = local_package.
    ENDIF.

  ENDMETHOD.

  METHOD get_supported_object_types.

    result = VALUE #( ( sign   = 'I'
                        option = 'EQ'
                        low    = type_package )
                      ( sign   = 'I'
                        option = 'EQ'
                        low    = type_class )
                      ( sign   = 'I'
                        option = 'EQ'
                        low    = type_interface )
                      ( sign   = 'I'
                        option = 'EQ'
                        low    = type_program )
                      ( sign   = 'I'
                        option = 'EQ'
                        low    = type_function_group )
                      ( sign   = 'I'
                        option = 'EQ'
                        low    = type_function_module )
                      ( sign   = 'I'
                        option = 'EQ'
                        low    = type_include ) ).
  ENDMETHOD.

ENDCLASS.
