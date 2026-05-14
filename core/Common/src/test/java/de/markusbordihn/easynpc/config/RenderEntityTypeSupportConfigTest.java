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

package de.markusbordihn.easynpc.config;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import java.util.HashSet;
import java.util.Set;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class RenderEntityTypeSupportConfigTest {

  @Nested
  @DisplayName("Entity Type List Format Validation")
  class FormatValidationTests {

    @Test
    @DisplayName("All known unsupported entity types should have valid format")
    void unsupportedEntityTypesShouldHaveValidFormat() {
      for (String entityType : RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_ENTITY_TYPES) {
        assertTrue(
            entityType.contains(":"),
            "Entity type should contain namespace separator ':' - found: " + entityType);
        assertFalse(
            entityType.startsWith(":"),
            "Entity type should not start with ':' - found: " + entityType);
        assertFalse(
            entityType.endsWith(":"), "Entity type should not end with ':' - found: " + entityType);
      }
    }

    @Test
    @DisplayName("All known supported entity types should have valid format")
    void supportedEntityTypesShouldHaveValidFormat() {
      for (String entityType : RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_ENTITY_TYPES) {
        assertTrue(
            entityType.contains(":"),
            "Entity type should contain namespace separator ':' - found: " + entityType);
        assertFalse(
            entityType.startsWith(":"),
            "Entity type should not start with ':' - found: " + entityType);
        assertFalse(
            entityType.endsWith(":"), "Entity type should not end with ':' - found: " + entityType);
      }
    }

    @Test
    @DisplayName("All known unsupported third-party entity types should have valid format")
    void unsupportedThirdPartyEntityTypesShouldHaveValidFormat() {
      for (String entityType :
          RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_THIRD_PARTY_ENTITY_TYPES) {
        assertTrue(
            entityType.contains(":"),
            "Entity type should contain namespace separator ':' - found: " + entityType);
        assertFalse(
            entityType.startsWith(":"),
            "Entity type should not start with ':' - found: " + entityType);
        assertFalse(
            entityType.endsWith(":"), "Entity type should not end with ':' - found: " + entityType);
        assertFalse(
            entityType.startsWith("minecraft:"),
            "Third-party entity type should not use minecraft namespace - found: " + entityType);
      }
    }

    @Test
    @DisplayName("All known supported third-party entity types should have valid format")
    void supportedThirdPartyEntityTypesShouldHaveValidFormat() {
      for (String entityType :
          RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_THIRD_PARTY_ENTITY_TYPES) {
        assertTrue(
            entityType.contains(":"),
            "Entity type should contain namespace separator ':' - found: " + entityType);
        assertFalse(
            entityType.startsWith(":"),
            "Entity type should not start with ':' - found: " + entityType);
        assertFalse(
            entityType.endsWith(":"), "Entity type should not end with ':' - found: " + entityType);
        assertFalse(
            entityType.startsWith("minecraft:"),
            "Third-party entity type should not use minecraft namespace - found: " + entityType);
      }
    }
  }

  @Nested
  @DisplayName("Entity Type List Consistency")
  class ConsistencyTests {

    @Test
    @DisplayName("No entity type should be in both supported and unsupported vanilla lists")
    void noOverlapBetweenVanillaSupportedAndUnsupported() {
      Set<String> overlap =
          new HashSet<>(RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_ENTITY_TYPES);
      overlap.retainAll(RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_ENTITY_TYPES);
      assertTrue(
          overlap.isEmpty(),
          "Entity types found in both supported and unsupported vanilla lists: " + overlap);
    }

    @Test
    @DisplayName("No entity type should be in both supported and unsupported third-party lists")
    void noOverlapBetweenThirdPartySupportedAndUnsupported() {
      Set<String> overlap =
          new HashSet<>(RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_THIRD_PARTY_ENTITY_TYPES);
      overlap.retainAll(RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_THIRD_PARTY_ENTITY_TYPES);
      assertTrue(
          overlap.isEmpty(),
          "Entity types found in both supported and unsupported third-party lists: " + overlap);
    }

    @Test
    @DisplayName("No overlap between vanilla and third-party supported lists")
    void noOverlapBetweenVanillaAndThirdPartySupported() {
      Set<String> overlap =
          new HashSet<>(RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_ENTITY_TYPES);
      overlap.retainAll(RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_THIRD_PARTY_ENTITY_TYPES);
      assertTrue(
          overlap.isEmpty(),
          "Entity types found in both vanilla and third-party supported lists: " + overlap);
    }

    @Test
    @DisplayName("No overlap between vanilla and third-party unsupported lists")
    void noOverlapBetweenVanillaAndThirdPartyUnsupported() {
      Set<String> overlap =
          new HashSet<>(RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_ENTITY_TYPES);
      overlap.retainAll(RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_THIRD_PARTY_ENTITY_TYPES);
      assertTrue(
          overlap.isEmpty(),
          "Entity types found in both vanilla and third-party unsupported lists: " + overlap);
    }
  }

  @Nested
  @DisplayName("Entity Type List Content Validation")
  class ContentValidationTests {

    @Test
    @DisplayName("All vanilla supported entity types should use minecraft namespace")
    void vanillaSupportedShouldUseMinecraftNamespace() {
      for (String entityType : RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_ENTITY_TYPES) {
        assertTrue(
            entityType.startsWith("minecraft:"),
            "Vanilla supported entity type should use minecraft namespace - found: " + entityType);
      }
    }

    @Test
    @DisplayName("All vanilla unsupported entity types should use minecraft namespace")
    void vanillaUnsupportedShouldUseMinecraftNamespace() {
      for (String entityType : RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_ENTITY_TYPES) {
        assertTrue(
            entityType.startsWith("minecraft:"),
            "Vanilla unsupported entity type should use minecraft namespace - found: "
                + entityType);
      }
    }

    @Test
    @DisplayName("Known supported vanilla entities should include common mobs")
    void shouldIncludeCommonVanillaMobs() {
      Set<String> entityTypes = RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_ENTITY_TYPES;
      assertTrue(entityTypes.contains("minecraft:zombie"), "Should contain zombie");
      assertTrue(entityTypes.contains("minecraft:skeleton"), "Should contain skeleton");
      assertTrue(entityTypes.contains("minecraft:creeper"), "Should contain creeper");
      assertTrue(entityTypes.contains("minecraft:villager"), "Should contain villager");
      assertTrue(entityTypes.contains("minecraft:iron_golem"), "Should contain iron_golem");
    }

    @Test
    @DisplayName("Known unsupported vanilla entities should include non-PathfinderMob entities")
    void shouldIncludeKnownUnsupportedVanilla() {
      Set<String> entityTypes = RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_ENTITY_TYPES;
      assertTrue(entityTypes.contains("minecraft:ender_dragon"), "Should contain ender_dragon");
      assertTrue(entityTypes.contains("minecraft:bat"), "Should contain bat");
      assertTrue(entityTypes.contains("minecraft:phantom"), "Should contain phantom");
    }

    @Test
    @DisplayName("Entity type lists should not be empty")
    void listsShouldNotBeEmpty() {
      assertFalse(RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_ENTITY_TYPES.isEmpty());
      assertFalse(RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_ENTITY_TYPES.isEmpty());
      assertFalse(
          RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_THIRD_PARTY_ENTITY_TYPES.isEmpty());
      assertFalse(
          RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_THIRD_PARTY_ENTITY_TYPES.isEmpty());
    }
  }

  @Nested
  @DisplayName("Name Pattern Filter Coverage for Unsupported Third-Party Entities")
  class PatternFilterCoverageTests {

    @Test
    @DisplayName("Unsupported third-party entities should not be already caught by name patterns")
    void unsupportedThirdPartyShouldNotBeRedundantWithPatternFilter() {
      Set<String> redundantEntries = new HashSet<>();
      for (String entityType :
          RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_THIRD_PARTY_ENTITY_TYPES) {
        if (EntityTypeManager.shouldFilterEntityTypeByName(entityType)) {
          redundantEntries.add(entityType);
        }
      }

      if (!redundantEntries.isEmpty()) {
        System.out.println(
            "INFO: "
                + redundantEntries.size()
                + " unsupported third-party entries are redundant with name pattern filter:");
        redundantEntries.stream().sorted().forEach(e -> System.out.println("  - " + e));
      }
    }

    @Test
    @DisplayName("Supported third-party entities caught by name pattern are documented")
    void supportedThirdPartyWithPatternConflictsAreDocumented() {
      Set<String> conflictingEntries = new HashSet<>();
      for (String entityType :
          RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_THIRD_PARTY_ENTITY_TYPES) {
        if (EntityTypeManager.shouldFilterEntityTypeByName(entityType)) {
          conflictingEntries.add(entityType);
        }
      }

      if (!conflictingEntries.isEmpty()) {
        System.out.println(
            "INFO: "
                + conflictingEntries.size()
                + " supported entities match name patterns (safe due to config priority):");
        conflictingEntries.stream().sorted().forEach(e -> System.out.println("  - " + e));
      }
    }

    @Test
    @DisplayName("Supported vanilla entities should NOT be caught by name pattern filter")
    void supportedVanillaShouldNeverBeFilteredByNamePattern() {
      for (String entityType : RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_ENTITY_TYPES) {
        assertFalse(
            EntityTypeManager.shouldFilterEntityTypeByName(entityType),
            "Supported entity type should NEVER be caught by name pattern filter: " + entityType);
      }
    }
  }
}
