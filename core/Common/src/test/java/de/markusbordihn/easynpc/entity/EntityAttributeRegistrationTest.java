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

package de.markusbordihn.easynpc.entity;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class EntityAttributeRegistrationTest {

  private static final double FLOAT_TOLERANCE = 1.0E-6D;

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static void assertBuildableAttributes(String label, AttributeSupplier.Builder builder) {
    assertNotNull(builder, label + " attributes builder");
    assertDoesNotThrow(builder::build, label + " attributes must build");
  }

  private static List<ModEntityTypeProvider> allEntityTypes() {
    List<ModEntityTypeProvider> entityTypes = new ArrayList<>();
    entityTypes.addAll(Arrays.asList(ModNPCEntityType.values()));
    entityTypes.addAll(Arrays.asList(ModRawEntityType.values()));
    entityTypes.addAll(Arrays.asList(ModCustomEntityType.values()));
    entityTypes.addAll(Arrays.asList(EpicFightEntityType.values()));
    entityTypes.addAll(Arrays.asList(CobblemonEntityType.values()));
    entityTypes.addAll(Arrays.asList(EasyModelEntitiesEntityType.values()));
    return entityTypes;
  }

  @Test
  @DisplayName("All common NPC entity types expose buildable attributes")
  void testNpcEntityTypesExposeAttributes() {
    for (ModNPCEntityType entityType : ModNPCEntityType.values()) {
      assertBuildableAttributes(entityType.name(), entityType.getAttributes());
    }
  }

  @Test
  @DisplayName("All raw entity types expose buildable attributes")
  void testRawEntityTypesExposeAttributes() {
    for (ModRawEntityType entityType : ModRawEntityType.values()) {
      assertBuildableAttributes(entityType.name(), entityType.getAttributes());
    }
  }

  @Test
  @DisplayName("All custom entity types expose buildable attributes")
  void testCustomEntityTypesExposeAttributes() {
    for (ModCustomEntityType entityType : ModCustomEntityType.values()) {
      assertBuildableAttributes(entityType.name(), entityType.getAttributes());
    }
  }

  @Test
  @DisplayName("All Epic Fight entity types expose buildable attributes")
  void testEpicFightEntityTypesExposeAttributes() {
    for (EpicFightEntityType entityType : EpicFightEntityType.values()) {
      assertBuildableAttributes(entityType.name(), entityType.getAttributes());
    }
  }

  @Test
  @DisplayName("All Cobblemon entity types expose buildable attributes")
  void testCobblemonEntityTypesExposeAttributes() {
    for (CobblemonEntityType entityType : CobblemonEntityType.values()) {
      assertBuildableAttributes(entityType.name(), entityType.getAttributes());
    }
  }

  @Test
  @DisplayName("All Easy Model Entities entity types expose buildable attributes")
  void testEasyModelEntitiesEntityTypesExposeAttributes() {
    for (EasyModelEntitiesEntityType entityType : EasyModelEntitiesEntityType.values()) {
      assertBuildableAttributes(entityType.name(), entityType.getAttributes());
    }
  }

  @Test
  @DisplayName("Every entity type can fly, so every entity type has a flying speed")
  void testEveryEntityTypeHasFlyingSpeed() {
    for (ModEntityTypeProvider entityType : allEntityTypes()) {
      assertTrue(
          ModEntityAttributes.buildWithNavigationAttributes(entityType)
              .hasAttribute(Attributes.FLYING_SPEED),
          entityType.getId() + " must have a flying speed");
    }
  }

  @Test
  @DisplayName("Tuned flying speeds are not overwritten by the default flying speed")
  void testTunedFlyingSpeedsAreKept() {
    Map<String, Double> tunedFlyingSpeeds =
        Map.of(
            "fairy", 0.6D,
            "vex", 0.6D,
            "ghast", 0.4D,
            "allay", 0.3D,
            "chicken", 0.3D);

    for (ModEntityTypeProvider entityType : allEntityTypes()) {
      Double tunedFlyingSpeed = tunedFlyingSpeeds.get(entityType.getId());
      if (tunedFlyingSpeed == null) {
        continue;
      }

      assertEquals(
          tunedFlyingSpeed,
          ModEntityAttributes.buildWithNavigationAttributes(entityType)
              .getValue(Attributes.FLYING_SPEED),
          FLOAT_TOLERANCE,
          entityType.getId() + " must keep its tuned flying speed");
    }
  }
}
