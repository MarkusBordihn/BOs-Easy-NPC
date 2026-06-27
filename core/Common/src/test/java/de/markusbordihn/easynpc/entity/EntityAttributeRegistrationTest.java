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
import static org.junit.jupiter.api.Assertions.assertNotNull;

import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class EntityAttributeRegistrationTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static void assertBuildableAttributes(String label, AttributeSupplier.Builder builder) {
    assertNotNull(builder, label + " attributes builder");
    assertDoesNotThrow(builder::build, label + " attributes must build");
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
}
