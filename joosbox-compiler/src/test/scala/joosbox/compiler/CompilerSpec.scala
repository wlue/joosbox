package joosbox.compiler.test

import org.specs2.mutable._
import joosbox.compiler._
import java.io.File


class FailCompilerSpec extends Specification {
  def getAllFiles(base: File): Array[File] = {
    base.listFiles.filter(_.getName.endsWith(".java")) ++ base.listFiles.filter(_.isDirectory).flatMap(getAllFiles)
  }

  def stdlibFilePaths: Seq[String] =
    getAllFiles(new File("joosbox-compiler/src/test/resources/stdlib/java")).map(_.getAbsolutePath)

  def filesForTest(name: String): Seq[String] = {
    val prefix = "joosbox-compiler/src/test/resources/marmoset-tests/a2/"
    if (name.endsWith(".java")) {
      Seq(new File(prefix + name).getAbsolutePath())
    } else {
      (getAllFiles(new File(prefix + name))).map(_.getAbsolutePath)
    }
  }

  "Type Checker" should {
      "Je_3_SingleTypeImport_ClashWithClass" in {
        val files = filesForTest("Je_3_SingleTypeImport_ClashWithClass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }
    }
}


class CompilerSpec extends Specification {

  def getAllFiles(base: File): Array[File] = {
    base.listFiles.filter(_.getName.endsWith(".java")) ++ base.listFiles.filter(_.isDirectory).flatMap(getAllFiles)
  }

  def stdlibFilePaths: Seq[String] =
    getAllFiles(new File("joosbox-compiler/src/test/resources/stdlib/java")).map(_.getAbsolutePath)

  def filesForTest(name: String): Seq[String] = {
    val prefix = "joosbox-compiler/src/test/resources/marmoset-tests/a2/"
    if (name.endsWith(".java")) {
      Seq(new File(prefix + name).getAbsolutePath())
    } else {
      (getAllFiles(new File(prefix + name))).map(_.getAbsolutePath)
    }
  }

  "Compiler" should {
    "pass marmoset tests for assignment 2" in {
      "J1_1_Cast_NamedTypeAsVariable.java" in {
        val files = filesForTest("J1_1_Cast_NamedTypeAsVariable.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_2_Fields_Case.java" in {
        val files = filesForTest("J1_2_Fields_Case.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_2_Locals_Overlapping_AfterBlock.java" in {
        val files = filesForTest("J1_2_Locals_Overlapping_AfterBlock.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_ImportOnDemand_DefaultImportInPresenceOfOtherImport" in {
        val files = filesForTest("J1_3_ImportOnDemand_DefaultImportInPresenceOfOtherImport") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_ImportOnDemand_ProgramDefinedPackage" in {
        val files = filesForTest("J1_3_ImportOnDemand_ProgramDefinedPackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_InfixResolvesToType" in {
        val files = filesForTest("J1_3_InfixResolvesToType") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_OnDemandImport_NonAmbiguous_Default" in {
        val files = filesForTest("J1_3_OnDemandImport_NonAmbiguous_Default") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_OnDemandImport_NonAmbiguous_SamePackage" in {
        val files = filesForTest("J1_3_OnDemandImport_NonAmbiguous_SamePackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_PackageClashWithType_Linked_Mutated" in {
        val files = filesForTest("J1_3_PackageClashWithType_Linked_Mutated") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_PackageDecl_MultipleFilesInSamePackage" in {
        val files = filesForTest("J1_3_PackageDecl_MultipleFilesInSamePackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_PackageDecl_SamePackageAndClassName" in {
        val files = filesForTest("J1_3_PackageDecl_SamePackageAndClassName") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_PackageExists_AsPrefix_External" in {
        val files = filesForTest("J1_3_PackageExists_AsPrefix_External") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_PackageExists_AsPrefix_Internal" in {
        val files = filesForTest("J1_3_PackageExists_AsPrefix_Internal") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_Resolve_LinkToCorrectPackage" in {
        val files = filesForTest("J1_3_Resolve_LinkToCorrectPackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_Resolve_PackagePrefixMatchClassName" in {
        val files = filesForTest("J1_3_Resolve_PackagePrefixMatchClassName") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_Resolve_SamePackage_External" in {
        val files = filesForTest("J1_3_Resolve_SamePackage_External") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_SingleTypeImport_ClashWithOnDemand" in {
        val files = filesForTest("J1_3_SingleTypeImport_ClashWithOnDemand") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_SingleTypeImport_ClashWithPackageName" in {
        val files = filesForTest("J1_3_SingleTypeImport_ClashWithPackageName") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_SingleTypeImport_ImportProgramClass" in {
        val files = filesForTest("J1_3_SingleTypeImport_ImportProgramClass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_SingleTypeImport_ImportSelf" in {
        val files = filesForTest("J1_3_SingleTypeImport_ImportSelf") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_SingleTypeImport_MultipleFromSamePackage" in {
        val files = filesForTest("J1_3_SingleTypeImport_MultipleFromSamePackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_SingleTypeImport_MultipleImportsOfSameType" in {
        val files = filesForTest("J1_3_SingleTypeImport_MultipleImportsOfSameType") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_3_SingleTypeImport_NoClash" in {
        val files = filesForTest("J1_3_SingleTypeImport_NoClash") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_AbstractMethod_InheritAbstractFromObject" in {
        val files = filesForTest("J1_4_AbstractMethod_InheritAbstractFromObject") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_AbstractMethod_InheritedFromInterface" in {
        val files = filesForTest("J1_4_AbstractMethod_InheritedFromInterface") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_ClassExtendsClass_SameName" in {
        val files = filesForTest("J1_4_ClassExtendsClass_SameName") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_ClassImplementsInterface_MultipleTimes" in {
        val files = filesForTest("J1_4_ClassImplementsInterface_MultipleTimes") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_Constructor_DuplicateArrayTypes.java" in {
        val files = filesForTest("J1_4_Constructor_DuplicateArrayTypes.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_Constructor_MatchAsSets.java" in {
        val files = filesForTest("J1_4_Constructor_MatchAsSets.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_DuplicateMethodDeclare_MethodNameEqualsConstructorName.java" in {
        val files = filesForTest("J1_4_DuplicateMethodDeclare_MethodNameEqualsConstructorName.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_InheritedFields_SameField_TwoWays" in {
        val files = filesForTest("J1_4_InheritedFields_SameField_TwoWays") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_InterfaceMethod_FromObject" in {
        val files = filesForTest("J1_4_InterfaceMethod_FromObject") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_MethodDeclare_DuplicateArrayTypes.java" in {
        val files = filesForTest("J1_4_MethodDeclare_DuplicateArrayTypes.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_Override_FinalOverrideNonFinal.java" in {
        val files = filesForTest("J1_4_Override_FinalOverrideNonFinal.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_Override_PublicOverridesProtected.java" in {
        val files = filesForTest("J1_4_Override_PublicOverridesProtected.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_PackageClashWithType_Loaded" in {
        val files = filesForTest("J1_4_PackageClashWithType_Loaded") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_PackageClashWithType_NotLoaded" in {
        val files = filesForTest("J1_4_PackageClashWithType_NotLoaded") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_PackageClashWithType_SingleTypeImport_DefaultPackage" in {
        val files = filesForTest("J1_4_PackageClashWithType_SingleTypeImport_DefaultPackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_PackageNameIsClassName_DefaultPackage" in {
        val files = filesForTest("J1_4_PackageNameIsClassName_DefaultPackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_Resolve_NotDefaultPackage" in {
        val files = filesForTest("J1_4_Resolve_NotDefaultPackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_4_SingleTypeImport_OnDemandsClash" in {
        val files = filesForTest("J1_4_SingleTypeImport_OnDemandsClash") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_6_ProtectedAccess_StaticMethod_This" in {
        val files = filesForTest("J1_6_ProtectedAccess_StaticMethod_This") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_abstract" in {
        val files = filesForTest("J1_abstract") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_access_override2.java" in {
        val files = filesForTest("J1_access_override2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_arbitrarylocaldeclaration.java" in {
        val files = filesForTest("J1_arbitrarylocaldeclaration.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_assign_Object_to_Object.java" in {
        val files = filesForTest("J1_assign_Object_to_Object.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_cast_to_same_type.java" in {
        val files = filesForTest("J1_cast_to_same_type.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_classextendsobject1.java" in {
        val files = filesForTest("J1_classextendsobject1.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_classextendsobject2.java" in {
        val files = filesForTest("J1_classextendsobject2.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_classimplementsserializable1" in {
        val files = filesForTest("J1_classimplementsserializable1") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_classimplementsserializable2" in {
        val files = filesForTest("J1_classimplementsserializable2") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_classimport" in {
        val files = filesForTest("J1_classimport") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_constructorWithSameNameAsMethod.java" in {
        val files = filesForTest("J1_constructorWithSameNameAsMethod.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_fields" in {
        val files = filesForTest("J1_fields") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_final_method_override1" in {
        val files = filesForTest("J1_final_method_override1") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_formal_with_same_name_as_field.java" in {
        val files = filesForTest("J1_formal_with_same_name_as_field.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_formalindex.java" in {
        val files = filesForTest("J1_formalindex.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_hierachyCheck14" in {
        val files = filesForTest("J1_hierachyCheck14") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_hierachyCheck28" in {
        val files = filesForTest("J1_hierachyCheck28") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_hierachyCheck29" in {
        val files = filesForTest("J1_hierachyCheck29") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_hierachyCheck31" in {
        val files = filesForTest("J1_hierachyCheck31") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_implicitsuper" in {
        val files = filesForTest("J1_implicitsuper") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_importName10.java" in {
        val files = filesForTest("J1_importName10.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_importName11.java" in {
        val files = filesForTest("J1_importName11.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_importName9.java" in {
        val files = filesForTest("J1_importName9.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_importNameLookup1" in {
        val files = filesForTest("J1_importNameLookup1") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_importNameLookup2" in {
        val files = filesForTest("J1_importNameLookup2") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_importNameLookup3" in {
        val files = filesForTest("J1_importNameLookup3") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_importNameLookup4" in {
        val files = filesForTest("J1_importNameLookup4") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_importNameLookup5" in {
        val files = filesForTest("J1_importNameLookup5") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_importNameLookup6" in {
        val files = filesForTest("J1_importNameLookup6") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_importNameLookup7" in {
        val files = filesForTest("J1_importNameLookup7") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_inherited_hashcode.java" in {
        val files = filesForTest("J1_inherited_hashcode.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_instance_method_hide1" in {
        val files = filesForTest("J1_instance_method_hide1") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_interfaceassignable" in {
        val files = filesForTest("J1_interfaceassignable") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_InterfaceObject" in {
        val files = filesForTest("J1_InterfaceObject") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_local_duplicate.java" in {
        val files = filesForTest("J1_local_duplicate.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_localvariablescope.java" in {
        val files = filesForTest("J1_localvariablescope.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_name" in {
        val files = filesForTest("J1_name") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_noduplicatefield.java" in {
        val files = filesForTest("J1_noduplicatefield.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_on_demand_imports_clash" in {
        val files = filesForTest("J1_on_demand_imports_clash") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_package" in {
        val files = filesForTest("J1_package") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_packageimport" in {
        val files = filesForTest("J1_packageimport") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_public_method_protected_override1" in {
        val files = filesForTest("J1_public_method_protected_override1") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_resolvetype2" in {
        val files = filesForTest("J1_resolvetype2") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_resolvetype3" in {
        val files = filesForTest("J1_resolvetype3") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_resolvetype4" in {
        val files = filesForTest("J1_resolvetype4") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_resolvetype6" in {
        val files = filesForTest("J1_resolvetype6") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_samePackage" in {
        val files = filesForTest("J1_samePackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_singleTypeImport" in {
        val files = filesForTest("J1_singleTypeImport") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_singleTypeImportSameTypeMultipleTimes" in {
        val files = filesForTest("J1_singleTypeImportSameTypeMultipleTimes") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_static_method_override1" in {
        val files = filesForTest("J1_static_method_override1") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_SubType1" in {
        val files = filesForTest("J1_SubType1") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_SubType2" in {
        val files = filesForTest("J1_SubType2") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_supermethod_override1" in {
        val files = filesForTest("J1_supermethod_override1") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_supermethod_override2" in {
        val files = filesForTest("J1_supermethod_override2") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_supermethod_override3" in {
        val files = filesForTest("J1_supermethod_override3") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_supermethod_override4" in {
        val files = filesForTest("J1_supermethod_override4") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_supermethod_override5" in {
        val files = filesForTest("J1_supermethod_override5") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_supermethod_override6" in {
        val files = filesForTest("J1_supermethod_override6") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J1_typecheck_assignment" in {
        val files = filesForTest("J1_typecheck_assignment") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_3_SingleTypeImport_ImportSelf_Interface" in {
        val files = filesForTest("J2_3_SingleTypeImport_ImportSelf_Interface") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_4_ImplementsInterface_TwiceByName" in {
        val files = filesForTest("J2_4_ImplementsInterface_TwiceByName") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_4_InterfaceExtends_MultipleWays" in {
        val files = filesForTest("J2_4_InterfaceExtends_MultipleWays") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_hierachyCheck22" in {
        val files = filesForTest("J2_hierachyCheck22") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_hierachyCheck23" in {
        val files = filesForTest("J2_hierachyCheck23") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_hierachyCheck24" in {
        val files = filesForTest("J2_hierachyCheck24") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_hierachyCheck25" in {
        val files = filesForTest("J2_hierachyCheck25") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_Ifaceimplicitabstract" in {
        val files = filesForTest("J2_Ifaceimplicitabstract") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_Interface1" in {
        val files = filesForTest("J2_Interface1") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_Interface10" in {
        val files = filesForTest("J2_Interface10") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_Interface11" in {
        val files = filesForTest("J2_Interface11") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_Interface2" in {
        val files = filesForTest("J2_Interface2") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_Interface3" in {
        val files = filesForTest("J2_Interface3") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_Interface6" in {
        val files = filesForTest("J2_Interface6") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_Interface7" in {
        val files = filesForTest("J2_Interface7") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_Interface8" in {
        val files = filesForTest("J2_Interface8") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_Interface9" in {
        val files = filesForTest("J2_Interface9") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "J2_interface_omitted_abstract" in {
        val files = filesForTest("J2_interface_omitted_abstract") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must not(throwA[Exception])
      }

      "Je_12_Fields_StaticNonStatic.java" in {
        val files = filesForTest("Je_12_Fields_StaticNonStatic.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_13_Interface_SingleImport_ClashWithClass" in {
        val files = filesForTest("Je_13_Interface_SingleImport_ClashWithClass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_14_Interface_DeclaresToString_DifferentReturnType" in {
        val files = filesForTest("Je_14_Interface_DeclaresToString_DifferentReturnType") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_14_Interface_DeclaresToString_ThrowsConflict" in {
        val files = filesForTest("Je_14_Interface_DeclaresToString_ThrowsConflict") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_14_Interface_DuplicateMethodDeclare.java" in {
        val files = filesForTest("Je_14_Interface_DuplicateMethodDeclare.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_14_Interface_ImplicitPublicMethod_ProtectedOverride" in {
        val files = filesForTest("Je_14_Interface_ImplicitPublicMethod_ProtectedOverride") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_14_Interface_SelfDependency_CyclicExtend" in {
        val files = filesForTest("Je_14_Interface_SelfDependency_CyclicExtend") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_14_Interface_SelfDependency_ExtendsItself.java" in {
        val files = filesForTest("Je_14_Interface_SelfDependency_ExtendsItself.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_ConstructorParameter_Duplicate.java" in {
        val files = filesForTest("Je_2_ConstructorParameter_Duplicate.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_ConstructorParameter_Overlapping.java" in {
        val files = filesForTest("Je_2_ConstructorParameter_Overlapping.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_DuplicateType" in {
        val files = filesForTest("Je_2_DuplicateType") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Fields_DifferentAccess.java" in {
        val files = filesForTest("Je_2_Fields_DifferentAccess.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Fields_DifferentTypes.java" in {
        val files = filesForTest("Je_2_Fields_DifferentTypes.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Fields_MultipleFields.java" in {
        val files = filesForTest("Je_2_Fields_MultipleFields.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Locals_Overlapping_DeeplyNested.java" in {
        val files = filesForTest("Je_2_Locals_Overlapping_DeeplyNested.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Locals_Overlapping_ForInitializer.java" in {
        val files = filesForTest("Je_2_Locals_Overlapping_ForInitializer.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Locals_Overlapping_InConditionalElse.java" in {
        val files = filesForTest("Je_2_Locals_Overlapping_InConditionalElse.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Locals_Overlapping_InConditionalThen.java" in {
        val files = filesForTest("Je_2_Locals_Overlapping_InConditionalThen.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Locals_Overlapping_InsideDoubleBlock.java" in {
        val files = filesForTest("Je_2_Locals_Overlapping_InsideDoubleBlock.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Locals_Overlapping_InsideLoop.java" in {
        val files = filesForTest("Je_2_Locals_Overlapping_InsideLoop.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Locals_Overlapping_InsideNewBlock.java" in {
        val files = filesForTest("Je_2_Locals_Overlapping_InsideNewBlock.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Locals_Overlapping_SameLevel.java" in {
        val files = filesForTest("Je_2_Locals_Overlapping_SameLevel.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Locals_Overlapping_SameLine.java" in {
        val files = filesForTest("Je_2_Locals_Overlapping_SameLine.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Parameter_AbstractDeclaredTwice.java" in {
        val files = filesForTest("Je_2_Parameter_AbstractDeclaredTwice.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Parameter_OverlappingWithLocal.java" in {
        val files = filesForTest("Je_2_Parameter_OverlappingWithLocal.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Parameter_OverlappingWithLocalInConditional.java" in {
        val files = filesForTest("Je_2_Parameter_OverlappingWithLocalInConditional.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Parameter_OverlappingWithLocalInLoop.java" in {
        val files = filesForTest("Je_2_Parameter_OverlappingWithLocalInLoop.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Parameter_OverlappingWithLocalInsideNewBlock.java" in {
        val files = filesForTest("Je_2_Parameter_OverlappingWithLocalInsideNewBlock.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Parameter_OverlappingWithLocalNotFirst.java" in {
        val files = filesForTest("Je_2_Parameter_OverlappingWithLocalNotFirst.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_2_Parameter_OverlappingWithParameter.java" in {
        val files = filesForTest("Je_2_Parameter_OverlappingWithParameter.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_ImportOnDemand_ClashWithImplicitImport" in {
        val files = filesForTest("Je_3_ImportOnDemand_ClashWithImplicitImport") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_ImportOnDemand_ClassInMultiplePackages" in {
        val files = filesForTest("Je_3_ImportOnDemand_ClassInMultiplePackages") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_ImportOnDemand_ClassNameAsPackage" in {
        val files = filesForTest("Je_3_ImportOnDemand_ClassNameAsPackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_ImportOnDemand_NonExisting.java" in {
        val files = filesForTest("Je_3_ImportOnDemand_NonExisting.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_ImportOnDemand_NonExistingPackage_FromPreviousTestcase.java" in {
        val files = filesForTest("Je_3_ImportOnDemand_NonExistingPackage_FromPreviousTestcase.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_ImportOnDemand_PackagePrefixExists" in {
        val files = filesForTest("Je_3_ImportOnDemand_PackagePrefixExists") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_PackageClashWithType_Explicit" in {
        val files = filesForTest("Je_3_PackageClashWithType_Explicit") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_PackageClashWithType_Linked" in {
        val files = filesForTest("Je_3_PackageClashWithType_Linked") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_PackageClashWithType_SingleTypeImport" in {
        val files = filesForTest("Je_3_PackageClashWithType_SingleTypeImport") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_PackageExists_AlmostPrefix_External" in {
        val files = filesForTest("Je_3_PackageExists_AlmostPrefix_External") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_PackageExists_AlmostPrefix_Internal" in {
        val files = filesForTest("Je_3_PackageExists_AlmostPrefix_Internal") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_PackageNameIsClassName" in {
        val files = filesForTest("Je_3_PackageNameIsClassName") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_PackageNameIsClassName_External" in {
        val files = filesForTest("Je_3_PackageNameIsClassName_External") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_PackageNameIsClassName_ExternalPrefix" in {
        val files = filesForTest("Je_3_PackageNameIsClassName_ExternalPrefix") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_PackageNameIsClassName_Prefix" in {
        val files = filesForTest("Je_3_PackageNameIsClassName_Prefix") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_Resolve_ImplicitJavaIO" in {
        val files = filesForTest("Je_3_Resolve_ImplicitJavaIO") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_Resolve_ImportDifferentFromSamePackage" in {
        val files = filesForTest("Je_3_Resolve_ImportDifferentFromSamePackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_Resolve_LinkToCorrectPackage" in {
        val files = filesForTest("Je_3_Resolve_LinkToCorrectPackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_Resolve_MissingImport" in {
        val files = filesForTest("Je_3_Resolve_MissingImport") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_Resolve_NonExistingSuperclass.java" in {
        val files = filesForTest("Je_3_Resolve_NonExistingSuperclass.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_Resolve_SamePackageAndClassName.java" in {
        val files = filesForTest("Je_3_Resolve_SamePackageAndClassName.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_SingleTypeImport_ClashWithClass" in {
        val files = filesForTest("Je_3_SingleTypeImport_ClashWithClass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_SingleTypeImport_ClashWithClass_InPackage" in {
        val files = filesForTest("Je_3_SingleTypeImport_ClashWithClass_InPackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_SingleTypeImport_ClashWithEachOther" in {
        val files = filesForTest("Je_3_SingleTypeImport_ClashWithEachOther") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_SingleTypeImport_ClashWithEachOther_MultipleImports" in {
        val files = filesForTest("Je_3_SingleTypeImport_ClashWithEachOther_MultipleImports") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_SingleTypeImport_ClashWithInterface" in {
        val files = filesForTest("Je_3_SingleTypeImport_ClashWithInterface") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_SingleTypeImport_NonExistingPackage" in {
        val files = filesForTest("Je_3_SingleTypeImport_NonExistingPackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_SingleTypeImport_NonExistingType" in {
        val files = filesForTest("Je_3_SingleTypeImport_NonExistingType") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_3_UndefinedType_DefaultPackageNotVisible" in {
        val files = filesForTest("Je_3_UndefinedType_DefaultPackageNotVisible") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_AbstractMethod_AbstractObjectMethods" in {
        val files = filesForTest("Je_4_AbstractMethod_AbstractObjectMethods") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_AbstractMethod_Declared.java" in {
        val files = filesForTest("Je_4_AbstractMethod_Declared.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_AbstractMethod_InheritFromInterface_1" in {
        val files = filesForTest("Je_4_AbstractMethod_InheritFromInterface_1") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_AbstractMethod_InheritFromInterface_2" in {
        val files = filesForTest("Je_4_AbstractMethod_InheritFromInterface_2") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_AbstractMethod_InheritFromSuperclass" in {
        val files = filesForTest("Je_4_AbstractMethod_InheritFromSuperclass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_AbstractMethod_InheritFromSuperclassInterface" in {
        val files = filesForTest("Je_4_AbstractMethod_InheritFromSuperclassInterface") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_AbstractMethod_InheritFromSuperInterface" in {
        val files = filesForTest("Je_4_AbstractMethod_InheritFromSuperInterface") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_AbstractMethod_NotAllImplemented" in {
        val files = filesForTest("Je_4_AbstractMethod_NotAllImplemented") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ClassExtendsCyclicClass" in {
        val files = filesForTest("Je_4_ClassExtendsCyclicClass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_DuplicateConstructor_Args.java" in {
        val files = filesForTest("Je_4_DuplicateConstructor_Args.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_DuplicateConstructor_ArrayArgs.java" in {
        val files = filesForTest("Je_4_DuplicateConstructor_ArrayArgs.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_DuplicateConstructor_NoArgs.java" in {
        val files = filesForTest("Je_4_DuplicateConstructor_NoArgs.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_DuplicateMethodDeclare_Args.java" in {
        val files = filesForTest("Je_4_DuplicateMethodDeclare_Args.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_DuplicateMethodDeclare_ArrayArgs.java" in {
        val files = filesForTest("Je_4_DuplicateMethodDeclare_ArrayArgs.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_DuplicateMethodDeclare_DifferentReturnTypes.java" in {
        val files = filesForTest("Je_4_DuplicateMethodDeclare_DifferentReturnTypes.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_DuplicateMethodDeclare_NoArgs.java" in {
        val files = filesForTest("Je_4_DuplicateMethodDeclare_NoArgs.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ExtendFinal.java" in {
        val files = filesForTest("Je_4_ExtendFinal.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ExtendNonClass" in {
        val files = filesForTest("Je_4_ExtendNonClass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_FinalHide" in {
        val files = filesForTest("Je_4_FinalHide") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_FinalOverride_DifferentReturnTypes.java" in {
        val files = filesForTest("Je_4_FinalOverride_DifferentReturnTypes.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_FinalOverride_SameSignature.java" in {
        val files = filesForTest("Je_4_FinalOverride_SameSignature.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_Hide_DifferentReturnTypes" in {
        val files = filesForTest("Je_4_Hide_DifferentReturnTypes") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ImplementNonInterface_Class.java" in {
        val files = filesForTest("Je_4_ImplementNonInterface_Class.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ImplementNonInterface_InterfaceAndClass" in {
        val files = filesForTest("Je_4_ImplementNonInterface_InterfaceAndClass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ImplementTwice_QualifiedName" in {
        val files = filesForTest("Je_4_ImplementTwice_QualifiedName") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ImplementTwice_SimpleName" in {
        val files = filesForTest("Je_4_ImplementTwice_SimpleName") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_InheritShadowsNonabstract" in {
        val files = filesForTest("Je_4_InheritShadowsNonabstract") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_Interface_FinalMethodFromObject" in {
        val files = filesForTest("Je_4_Interface_FinalMethodFromObject") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_InterfaceExtendsCyclicInterface" in {
        val files = filesForTest("Je_4_InterfaceExtendsCyclicInterface") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_Override_DifferentReturnTypes_AbstractFromSuperclassAndInterface" in {
        val files = filesForTest("Je_4_Override_DifferentReturnTypes_AbstractFromSuperclassAndInterface") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_Override_DifferentReturnTypes_FromSuperclassAndInterface" in {
        val files = filesForTest("Je_4_Override_DifferentReturnTypes_FromSuperclassAndInterface") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_Override_DifferentReturnTypes_FromSuperclassAndInterface_NonVoid" in {
        val files = filesForTest("Je_4_Override_DifferentReturnTypes_FromSuperclassAndInterface_NonVoid") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_Override_DifferentReturnTypes_TwoInterfaces" in {
        val files = filesForTest("Je_4_Override_DifferentReturnTypes_TwoInterfaces") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_Override_DifferentReturnTypesFromInterface" in {
        val files = filesForTest("Je_4_Override_DifferentReturnTypesFromInterface") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ProtectedHide_FromSuperclass" in {
        val files = filesForTest("Je_4_ProtectedHide_FromSuperclass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ProtectedOverride_Abstract" in {
        val files = filesForTest("Je_4_ProtectedOverride_Abstract") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ProtectedOverride_DifferentThrows" in {
        val files = filesForTest("Je_4_ProtectedOverride_DifferentThrows") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ProtectedOverride_Exception_Clone" in {
        val files = filesForTest("Je_4_ProtectedOverride_Exception_Clone") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ProtectedOverride_FromInterface" in {
        val files = filesForTest("Je_4_ProtectedOverride_FromInterface") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ProtectedOverride_FromSuperclass" in {
        val files = filesForTest("Je_4_ProtectedOverride_FromSuperclass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ProtectedOverride_FromSuperclassAndInterface" in {
        val files = filesForTest("Je_4_ProtectedOverride_FromSuperclassAndInterface") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ProtectedOverride_TwoVersionsFromSuperclass" in {
        val files = filesForTest("Je_4_ProtectedOverride_TwoVersionsFromSuperclass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ReplaceInstance_FromSuperclass.java" in {
        val files = filesForTest("Je_4_ReplaceInstance_FromSuperclass.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ReplaceStatic_FromSuperclass" in {
        val files = filesForTest("Je_4_ReplaceStatic_FromSuperclass") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_ReplaceStatic_FromSuperclass_DifferentReturnTypes" in {
        val files = filesForTest("Je_4_ReplaceStatic_FromSuperclass_DifferentReturnTypes") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_Resolve_DefaultPackage" in {
        val files = filesForTest("Je_4_Resolve_DefaultPackage") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_SelfDependency_CircularExtends_1" in {
        val files = filesForTest("Je_4_SelfDependency_CircularExtends_1") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_SelfDependency_CircularExtends_2" in {
        val files = filesForTest("Je_4_SelfDependency_CircularExtends_2") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_4_SelfDependency_ExtendsItself.java" in {
        val files = filesForTest("Je_4_SelfDependency_ExtendsItself.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }

      "Je_5_Interface_ImplicitReplace_DifferentReturnType.java" in {
        val files = filesForTest("Je_5_Interface_ImplicitReplace_DifferentReturnType.java") ++ stdlibFilePaths
        CompilerRunner.runTestable(files.toArray) must throwA[Exception]
      }
    }
  }
}
