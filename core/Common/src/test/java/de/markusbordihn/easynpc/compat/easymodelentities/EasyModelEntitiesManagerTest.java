/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.compat.easymodelentities;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelType;
import net.minecraft.resources.Identifier;
import org.junit.jupiter.api.Test;

class EasyModelEntitiesManagerTest {

  @Test
  void getProfileIdParsesValidProfile() {
    assertEquals(
        Identifier.fromNamespaceAndPath("my_pack", "my_model"),
        EasyModelEntitiesManager.getProfileId("my_pack:my_model"));
  }

  @Test
  void getProfileIdFallsBackToDefaultProfile() {
    Identifier defaultProfile = Identifier.parse(EasyModelEntitiesManager.DEFAULT_PROFILE);
    assertEquals(defaultProfile, EasyModelEntitiesManager.getProfileId(null));
    assertEquals(defaultProfile, EasyModelEntitiesManager.getProfileId(""));
    assertEquals(defaultProfile, EasyModelEntitiesManager.getProfileId("invalid profile id!"));
  }

  @Test
  void isFloatingProfileDetectsFloatingBodyType() {
    Identifier floatingProfile = Identifier.fromNamespaceAndPath("my_pack", "wisp");
    Identifier groundProfile = Identifier.fromNamespaceAndPath("my_pack", "explorer");
    EasyModelEntitiesManager.registerProfileBodyType(floatingProfile, "FLOATING");
    EasyModelEntitiesManager.registerProfileBodyType(groundProfile, "BIPED");

    assertTrue(EasyModelEntitiesManager.isFloatingProfile(floatingProfile));
    assertFalse(EasyModelEntitiesManager.isFloatingProfile(groundProfile));
    assertFalse(
        EasyModelEntitiesManager.isFloatingProfile(
            Identifier.fromNamespaceAndPath("my_pack", "unknown")));

    EasyModelEntitiesManager.clearProfileModelTypes();
    assertFalse(EasyModelEntitiesManager.isFloatingProfile(floatingProfile));
  }

  @Test
  void profileDimensionsAreRegisteredAndCleared() {
    Identifier profileId = Identifier.fromNamespaceAndPath("my_pack", "large_model");
    EasyModelEntitiesManager.registerProfileDimensions(profileId, 1.5F, 3.0F, 2.6F);

    EasyModelEntitiesManager.ProfileDimensions dimensions =
        EasyModelEntitiesManager.getProfileDimensions(profileId);
    assertEquals(1.5F, dimensions.width());
    assertEquals(3.0F, dimensions.height());
    assertEquals(2.6F, dimensions.eyeHeight());

    EasyModelEntitiesManager.clearProfileModelTypes();
    assertNull(EasyModelEntitiesManager.getProfileDimensions(profileId));
  }

  @Test
  void clearingClientModelMetadataPreservesServerDimensions() {
    Identifier profileId = Identifier.fromNamespaceAndPath("my_pack", "floating_model");
    EasyModelEntitiesManager.registerProfileModelType(profileId, ModelType.SLIME);
    EasyModelEntitiesManager.registerProfileBodyType(profileId, "FLOATING");
    EasyModelEntitiesManager.registerProfileDimensions(profileId, 1.0F, 2.0F, 1.5F);

    EasyModelEntitiesManager.clearProfileModelMetadata();

    assertEquals(ModelType.HUMANOID, EasyModelEntitiesManager.getProfileModelType(profileId));
    assertNull(EasyModelEntitiesManager.getProfileBodyType(profileId));
    assertEquals(
        new EasyModelEntitiesManager.ProfileDimensions(1.0F, 2.0F, 1.5F),
        EasyModelEntitiesManager.getProfileDimensions(profileId));
    EasyModelEntitiesManager.clearProfileModelTypes();
  }

  @Test
  void getModelTypeMapsBodyTypes() {
    assertEquals(ModelType.HUMANOID, EasyModelEntitiesManager.getModelType("BIPED"));
    assertEquals(ModelType.PIXIE, EasyModelEntitiesManager.getModelType("WINGED_HUMANOID"));
    assertEquals(ModelType.QUADRUPED, EasyModelEntitiesManager.getModelType("QUADRUPED"));
    assertEquals(ModelType.QUADRUPED, EasyModelEntitiesManager.getModelType("AMPHIBIOUS"));
    assertEquals(ModelType.QUADRUPED, EasyModelEntitiesManager.getModelType("AQUATIC"));
    assertEquals(ModelType.AVIAN, EasyModelEntitiesManager.getModelType("WINGED"));
    assertEquals(ModelType.SPIDER, EasyModelEntitiesManager.getModelType("ARTHROPOD"));
    assertEquals(ModelType.SLIME, EasyModelEntitiesManager.getModelType("STATIC"));
    assertEquals(ModelType.SLIME, EasyModelEntitiesManager.getModelType("CUBOID"));
    assertEquals(ModelType.SLIME, EasyModelEntitiesManager.getModelType("FLOATING"));
  }

  @Test
  void getModelTypeFallsBackToHumanoid() {
    assertEquals(ModelType.HUMANOID, EasyModelEntitiesManager.getModelType(null));
    assertEquals(ModelType.HUMANOID, EasyModelEntitiesManager.getModelType(""));
    assertEquals(ModelType.HUMANOID, EasyModelEntitiesManager.getModelType("UNKNOWN_BODY_TYPE"));
  }

  @Test
  void getModelPartTypeMapsCanonicalParts() {
    assertEquals(ModelPartType.HEAD, EasyModelEntitiesManager.getModelPartType("head"));
    assertEquals(ModelPartType.BODY, EasyModelEntitiesManager.getModelPartType("body"));
    assertEquals(ModelPartType.LEFT_ARM, EasyModelEntitiesManager.getModelPartType("left_arm"));
    assertEquals(ModelPartType.RIGHT_ARM, EasyModelEntitiesManager.getModelPartType("right_arm"));
    assertEquals(ModelPartType.LEFT_LEG, EasyModelEntitiesManager.getModelPartType("left_leg"));
    assertEquals(ModelPartType.RIGHT_LEG, EasyModelEntitiesManager.getModelPartType("right_leg"));
    assertEquals(ModelPartType.LEFT_WING, EasyModelEntitiesManager.getModelPartType("left_wing"));
    assertEquals(ModelPartType.RIGHT_WING, EasyModelEntitiesManager.getModelPartType("right_wing"));
    assertEquals(ModelPartType.TAIL, EasyModelEntitiesManager.getModelPartType("tail"));
  }

  @Test
  void getModelPartTypeMapsQuadrupedAndFinParts() {
    assertEquals(
        ModelPartType.LEFT_FRONT_LEG, EasyModelEntitiesManager.getModelPartType("front_left_leg"));
    assertEquals(
        ModelPartType.RIGHT_FRONT_LEG,
        EasyModelEntitiesManager.getModelPartType("front_right_leg"));
    assertEquals(
        ModelPartType.LEFT_HIND_LEG, EasyModelEntitiesManager.getModelPartType("back_left_leg"));
    assertEquals(
        ModelPartType.RIGHT_HIND_LEG, EasyModelEntitiesManager.getModelPartType("back_right_leg"));
    assertEquals(ModelPartType.TAIL2, EasyModelEntitiesManager.getModelPartType("tail_fin"));
  }

  @Test
  void getModelPartTypeFallsBackToUnknown() {
    assertEquals(ModelPartType.UNKNOWN, EasyModelEntitiesManager.getModelPartType(null));
    assertEquals(ModelPartType.UNKNOWN, EasyModelEntitiesManager.getModelPartType(""));
    assertEquals(ModelPartType.UNKNOWN, EasyModelEntitiesManager.getModelPartType("antenna_left"));
  }
}
