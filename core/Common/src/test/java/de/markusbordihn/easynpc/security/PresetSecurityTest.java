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

package de.markusbordihn.easynpc.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.preset.PresetType;
import net.minecraft.resources.Identifier;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class PresetSecurityTest {

  private static boolean isAllowed(PresetType presetType, String resourceLocation) {
    return PresetSecurity.validateResourceLocation(
            presetType, Identifier.tryParse(resourceLocation))
        .allowed();
  }

  private static PresetType resolveType(PresetType presetType, String resourceLocation) {
    return PresetSecurity.resolveResourcePresetType(Identifier.parse(resourceLocation), presetType);
  }

  @Test
  @DisplayName("A third party mod can ship data presets below its own easy_npc folder")
  void testThirdPartyNamespaceIsAllowed() {
    assertTrue(isAllowed(PresetType.DATA, "othermod:easy_npc/preset/companion.npc.nbt"));
    assertTrue(isAllowed(PresetType.DATA, "othermod:easy_npc/preset/nested/companion.npc.snbt"));
  }

  @Test
  @DisplayName("The generic preset folder stays reserved for the own namespace")
  void testGenericPresetFolderIsNamespaceLocked() {
    assertTrue(isAllowed(PresetType.DATA, "easy_npc:preset/humanoid/villager.npc.nbt"));
    assertFalse(isAllowed(PresetType.DATA, "othermod:preset/unrelated_mod_data.npc.nbt"));
  }

  @Test
  @DisplayName("Default presets stay exclusive to the own namespace")
  void testDefaultPresetsAreNamespaceLocked() {
    assertTrue(isAllowed(PresetType.DEFAULT, "easy_npc:default_preset/villager/builder.npc.snbt"));
    assertFalse(isAllowed(PresetType.DEFAULT, "othermod:default_preset/builder.npc.snbt"));
    assertFalse(isAllowed(PresetType.DEFAULT, "othermod:easy_npc/default_preset/builder.npc.snbt"));
  }

  @Test
  @DisplayName("Presets outside of the preset folder or with an unknown format are rejected")
  void testInvalidResourcesAreRejected() {
    assertFalse(isAllowed(PresetType.DATA, "othermod:easy_npc/preset/companion.txt"));
    assertFalse(isAllowed(PresetType.DATA, "othermod:easy_npc/companion.npc.nbt"));
    assertFalse(isAllowed(PresetType.DATA, "othermod:companion.npc.nbt"));
  }

  @Test
  @DisplayName("Preset types that are not read from resources are not path checked")
  void testNonResourcePresetTypesAreNotPathChecked() {
    assertTrue(isAllowed(PresetType.CUSTOM, "othermod:anywhere/companion.npc.nbt"));
    assertTrue(isAllowed(PresetType.WORLD, "othermod:anywhere/companion.npc.nbt"));
    assertTrue(isAllowed(PresetType.LOCAL, "othermod:anywhere/companion.npc.nbt"));
  }

  @Test
  @DisplayName("A preset from resources is read as the type its folder belongs to")
  void testResourcePresetTypeIsResolvedByFolder() {
    assertEquals(
        PresetType.DEFAULT,
        resolveType(PresetType.DATA, "easy_npc:default_preset/villager/builder.npc.snbt"));
    assertEquals(
        PresetType.DATA,
        resolveType(PresetType.DEFAULT, "othermod:easy_npc/preset/companion.npc.nbt"));
    assertEquals(
        PresetType.DATA, resolveType(PresetType.DATA, "easy_npc:preset/humanoid/villager.npc.nbt"));
  }

  @Test
  @DisplayName("A preset outside of the resource folders keeps its own type and stays blocked")
  void testUnknownResourcePathKeepsItsType() {
    assertEquals(
        PresetType.DATA, resolveType(PresetType.DATA, "othermod:anywhere/companion.npc.nbt"));
    assertEquals(
        PresetType.CUSTOM, resolveType(PresetType.CUSTOM, "easy_npc:default_preset/x.npc.snbt"));
    assertFalse(isAllowed(PresetType.DATA, "othermod:anywhere/companion.npc.nbt"));
  }
}
