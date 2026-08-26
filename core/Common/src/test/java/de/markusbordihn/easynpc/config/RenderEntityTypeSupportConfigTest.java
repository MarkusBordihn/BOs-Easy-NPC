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
import java.util.Properties;
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
    @DisplayName("Known unsupported vanilla entities should include non-renderable entities")
    void shouldIncludeKnownUnsupportedVanilla() {
      Set<String> entityTypes = RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_ENTITY_TYPES;
      assertTrue(entityTypes.contains("minecraft:ender_dragon"), "Should contain ender_dragon");
      assertTrue(entityTypes.contains("minecraft:phantom"), "Should contain phantom");
    }

    @Test
    @DisplayName("The bat is supported and no longer listed as unsupported")
    void shouldSupportBat() {
      assertTrue(
          RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_ENTITY_TYPES.contains("minecraft:bat"),
          "Should contain bat");
      assertFalse(
          RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_ENTITY_TYPES.contains("minecraft:bat"),
          "Should no longer list bat as unsupported");
      assertFalse(
          EntityTypeManager.shouldFilterEntityTypeByName("minecraft:bat"),
          "Should not be caught by the name pattern filter");
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
  @DisplayName("Changed Default Migration")
  class ChangedDefaultMigrationTests {

    @Test
    @DisplayName("An outdated config file drops the stored value of a changed default")
    void shouldResetStoredValueOfChangedDefault() {
      Properties properties = new Properties();
      properties.setProperty("minecraft:bat", "false");
      properties.setProperty("minecraft:zombie", "false");

      RenderEntityTypeSupportConfig.resetEntityTypesWithChangedDefault(properties);

      assertFalse(properties.containsKey("minecraft:bat"), "Should drop the outdated bat entry");
      assertEquals(
          "false", properties.getProperty("minecraft:zombie"), "Should keep unrelated entries");
      assertEquals(
          Integer.toString(RenderEntityTypeSupportConfig.CONFIG_VERSION),
          properties.getProperty(RenderEntityTypeSupportConfig.CONFIG_VERSION_KEY),
          "Should stamp the current config version");
    }

    @Test
    @DisplayName("An up-to-date config file keeps every stored value")
    void shouldKeepStoredValuesOfCurrentConfigVersion() {
      Properties properties = new Properties();
      properties.setProperty(
          RenderEntityTypeSupportConfig.CONFIG_VERSION_KEY,
          Integer.toString(RenderEntityTypeSupportConfig.CONFIG_VERSION));
      properties.setProperty("minecraft:bat", "false");

      RenderEntityTypeSupportConfig.resetEntityTypesWithChangedDefault(properties);

      assertEquals(
          "false",
          properties.getProperty("minecraft:bat"),
          "Should keep a value the user changed after the migration");
    }

    @Test
    @DisplayName("Every changed default is listed as supported or unsupported")
    void changedDefaultsShouldBeKnownEntityTypes() {
      for (String entityType : RenderEntityTypeSupportDefaults.ENTITY_TYPES_WITH_CHANGED_DEFAULT) {
        assertTrue(
            RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_ENTITY_TYPES.contains(entityType)
                || RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_ENTITY_TYPES.contains(
                    entityType),
            "Changed default should be a known entity type - found: " + entityType);
      }
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

      assertTrue(
          redundantEntries.isEmpty(),
          "Unsupported third-party entries are already covered by the name pattern filter and "
              + "should be removed from the list: "
              + redundantEntries.stream().sorted().toList());
    }

    @Test
    @DisplayName("Only documented supported third-party entities are caught by name patterns")
    void supportedThirdPartyWithPatternConflictsAreDocumented() {
      Set<String> conflictingEntries = new HashSet<>();
      for (String entityType :
          RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_THIRD_PARTY_ENTITY_TYPES) {
        if (EntityTypeManager.shouldFilterEntityTypeByName(entityType)) {
          conflictingEntries.add(entityType);
        }
      }

      assertEquals(
          Set.of("ob_core:obsidian_shard", "simple_mobs:imp_bomb", "simple_mobs:tail_dart"),
          conflictingEntries,
          "A new name pattern hides a supported entity type, which only the explicit config entry "
              + "still re-enables");
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
