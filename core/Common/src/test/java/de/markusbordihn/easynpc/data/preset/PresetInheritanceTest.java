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

package de.markusbordihn.easynpc.data.preset;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.entity.Entity;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class PresetInheritanceTest {

  private static final ResourceLocation CHILD = ResourceLocation.fromNamespaceAndPath("test_mod", "child");
  private static final ResourceLocation PARENT = ResourceLocation.fromNamespaceAndPath("test_mod", "parent");
  private static final ResourceLocation GRANDPARENT =
      ResourceLocation.fromNamespaceAndPath("test_mod", "grandparent");

  private final Map<ResourceLocation, CompoundTag> presets = new HashMap<>();
  private final Function<ResourceLocation, CompoundTag> presetLoader = this.presets::get;

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static CompoundTag flatPreset(String entityId) {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putString(Entity.ID_TAG, entityId);
    return compoundTag;
  }

  private static CompoundTag wrappedPreset(String entityId) {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.put(PresetData.DATA_TAG, flatPreset(entityId));
    return compoundTag;
  }

  private static CompoundTag withParent(CompoundTag compoundTag, ResourceLocation parentLocation) {
    compoundTag.putString(PresetData.PARENT_TAG, parentLocation.toString());
    return compoundTag;
  }

  private static ResourceLocation chainLocation(int index) {
    return ResourceLocation.fromNamespaceAndPath("test_mod", "chain_" + index);
  }

  private static String chainTag(int index) {
    return "Chain" + index;
  }

  @Test
  @DisplayName("A preset without a parent is returned unchanged")
  void testPresetWithoutParent() {
    CompoundTag compoundTag = flatPreset("easy_npc:humanoid");

    assertEquals(compoundTag, PresetInheritance.resolve(compoundTag, CHILD, this.presetLoader));
  }

  @Test
  @DisplayName("The child overrides the parent and keeps the values it does not define")
  void testChildOverridesParent() {
    CompoundTag parentTag = flatPreset("easy_npc:humanoid");
    parentTag.putString("CustomName", "Parent");
    parentTag.putBoolean("NoGravity", true);
    this.presets.put(PARENT, parentTag);

    CompoundTag childTag = withParent(flatPreset("easy_npc:humanoid"), PARENT);
    childTag.putString("CustomName", "Child");

    CompoundTag resolvedTag = PresetInheritance.resolve(childTag, CHILD, this.presetLoader);

    assertEquals("Child", resolvedTag.getString("CustomName"));
    assertTrue(resolvedTag.getBoolean("NoGravity"));
    assertFalse(resolvedTag.contains(PresetData.PARENT_TAG));
  }

  @Test
  @DisplayName("The entity UUID of the parent is not inherited, so siblings stay separate NPCs")
  void testIdentityIsNotInherited() {
    CompoundTag parentTag = flatPreset("easy_npc:humanoid");
    parentTag.putString(Entity.UUID_TAG, "9a1b2c3d-0000-0000-0000-000000000001");
    parentTag.putString(PresetData.PRESET_UUID_TAG, "9a1b2c3d-0000-0000-0000-000000000002");
    this.presets.put(PARENT, parentTag);

    CompoundTag resolvedTag =
        PresetInheritance.resolve(
            withParent(flatPreset("easy_npc:humanoid"), PARENT), CHILD, this.presetLoader);

    assertFalse(resolvedTag.contains(Entity.UUID_TAG));
    assertFalse(resolvedTag.contains(PresetData.PRESET_UUID_TAG));
  }

  @Test
  @DisplayName("The child keeps its own entity UUID")
  void testChildIdentityIsKept() {
    CompoundTag parentTag = flatPreset("easy_npc:humanoid");
    parentTag.putString(Entity.UUID_TAG, "9a1b2c3d-0000-0000-0000-000000000001");
    this.presets.put(PARENT, parentTag);

    CompoundTag childTag = withParent(flatPreset("easy_npc:humanoid"), PARENT);
    childTag.putString(Entity.UUID_TAG, "9a1b2c3d-0000-0000-0000-00000000000c");

    CompoundTag resolvedTag = PresetInheritance.resolve(childTag, CHILD, this.presetLoader);

    assertEquals("9a1b2c3d-0000-0000-0000-00000000000c", resolvedTag.getString(Entity.UUID_TAG));
  }

  @Test
  @DisplayName("The identity inside a wrapped preset is not inherited either")
  void testIdentityIsNotInheritedFromWrappedPreset() {
    CompoundTag parentTag = wrappedPreset("easy_npc:humanoid");
    CompoundTag parentEntityData = flatPreset("easy_npc:humanoid");
    parentEntityData.putString(Entity.UUID_TAG, "9a1b2c3d-0000-0000-0000-000000000001");
    parentEntityData.putString("CustomName", "Parent");
    parentTag.put(PresetData.DATA_TAG, parentEntityData);
    this.presets.put(PARENT, parentTag);

    CompoundTag resolvedTag =
        PresetInheritance.resolve(
            withParent(wrappedPreset("easy_npc:humanoid"), PARENT), CHILD, this.presetLoader);
    CompoundTag resolvedEntityData = resolvedTag.getCompound(PresetData.DATA_TAG);

    assertFalse(resolvedEntityData.contains(Entity.UUID_TAG));
    assertEquals("Parent", resolvedEntityData.getString("CustomName"));
  }

  @Test
  @DisplayName("A chain of parents is merged from the top down")
  void testParentChain() {
    CompoundTag grandparentTag = flatPreset("easy_npc:humanoid");
    grandparentTag.putString("CustomName", "Grandparent");
    grandparentTag.putString("Team", "guards");
    this.presets.put(GRANDPARENT, grandparentTag);

    CompoundTag parentTag = withParent(flatPreset("easy_npc:humanoid"), GRANDPARENT);
    parentTag.putString("CustomName", "Parent");
    this.presets.put(PARENT, parentTag);

    CompoundTag resolvedTag =
        PresetInheritance.resolve(
            withParent(flatPreset("easy_npc:humanoid"), PARENT), CHILD, this.presetLoader);

    assertEquals("Parent", resolvedTag.getString("CustomName"));
    assertEquals("guards", resolvedTag.getString("Team"));
  }

  @Test
  @DisplayName("A child of another NPC type inherits the logic but not the look")
  void testEntityTypeSpecificDataIsNotInherited() {
    CompoundTag parentTag = flatPreset("easy_npc:skeleton");
    parentTag.putString(VariantDataCapable.EASY_NPC_DATA_VARIANT_TYPE_TAG, "SKELETON_01");
    parentTag.putString(ProfessionDataCapable.DATA_PROFESSION_TAG, "NONE");
    parentTag.put(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG, new CompoundTag());
    parentTag.put(ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG, new CompoundTag());
    parentTag.put("DialogData", new CompoundTag());
    parentTag.putString("CustomName", "Parent");
    this.presets.put(PARENT, parentTag);

    CompoundTag resolvedTag =
        PresetInheritance.resolve(
            withParent(flatPreset("easy_npc:humanoid"), PARENT), CHILD, this.presetLoader);

    assertFalse(resolvedTag.contains(VariantDataCapable.EASY_NPC_DATA_VARIANT_TYPE_TAG));
    assertFalse(resolvedTag.contains(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG));
    assertFalse(resolvedTag.contains(ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG));
    assertFalse(resolvedTag.contains(ProfessionDataCapable.DATA_PROFESSION_TAG));
    assertTrue(resolvedTag.contains("DialogData"));
    assertEquals("Parent", resolvedTag.getString("CustomName"));
    assertEquals("easy_npc:humanoid", resolvedTag.getString(Entity.ID_TAG));
  }

  @Test
  @DisplayName("A child of the same NPC type still inherits the look")
  void testEntityTypeSpecificDataIsInheritedForTheSameType() {
    CompoundTag parentTag = flatPreset("easy_npc:humanoid");
    parentTag.putString(VariantDataCapable.EASY_NPC_DATA_VARIANT_TYPE_TAG, "KNIGHT_01");
    this.presets.put(PARENT, parentTag);

    CompoundTag resolvedTag =
        PresetInheritance.resolve(
            withParent(flatPreset("easy_npc:humanoid"), PARENT), CHILD, this.presetLoader);

    assertEquals(
        "KNIGHT_01", resolvedTag.getString(VariantDataCapable.EASY_NPC_DATA_VARIANT_TYPE_TAG));
  }

  @Test
  @DisplayName("A child without an own NPC type keeps the type and the look of its parent")
  void testChildWithoutEntityTypeInheritsEverything() {
    CompoundTag parentTag = flatPreset("easy_npc:humanoid");
    parentTag.putString(VariantDataCapable.EASY_NPC_DATA_VARIANT_TYPE_TAG, "KNIGHT_01");
    this.presets.put(PARENT, parentTag);

    CompoundTag childTag = new CompoundTag();
    childTag.putString("CustomName", "Child");
    CompoundTag resolvedTag =
        PresetInheritance.resolve(withParent(childTag, PARENT), CHILD, this.presetLoader);

    assertEquals("easy_npc:humanoid", resolvedTag.getString(Entity.ID_TAG));
    assertEquals(
        "KNIGHT_01", resolvedTag.getString(VariantDataCapable.EASY_NPC_DATA_VARIANT_TYPE_TAG));
  }

  @Test
  @DisplayName("The preview data of the parent is dropped on a type change")
  void testPreviewMetadataIsNotInheritedOnTypeChange() {
    CompoundTag parentTag = wrappedPreset("easy_npc:skeleton");
    CompoundTag parentMetadata = new CompoundTag();
    parentMetadata.putString(PresetMetadata.TAG_ENTITY_TYPE_ID, "easy_npc:skeleton");
    parentMetadata.putString(PresetMetadata.TAG_VARIANT_TYPE, "SKELETON_01");
    parentMetadata.putString(PresetMetadata.TAG_AUTHOR, "Kaworru");
    parentTag.put(PresetDataCapable.PRESET_METADATA_TAG, parentMetadata);
    this.presets.put(PARENT, parentTag);

    CompoundTag resolvedTag =
        PresetInheritance.resolve(
            withParent(wrappedPreset("easy_npc:humanoid"), PARENT), CHILD, this.presetLoader);
    CompoundTag resolvedMetadata = resolvedTag.getCompound(PresetDataCapable.PRESET_METADATA_TAG);

    assertFalse(resolvedMetadata.contains(PresetMetadata.TAG_ENTITY_TYPE_ID));
    assertFalse(resolvedMetadata.contains(PresetMetadata.TAG_VARIANT_TYPE));
    assertEquals("Kaworru", resolvedMetadata.getString(PresetMetadata.TAG_AUTHOR));
  }

  @Test
  void testCycleIsRejected() {
    this.presets.put(PARENT, withParent(flatPreset("easy_npc:humanoid"), CHILD));

    assertNull(
        PresetInheritance.resolve(
            withParent(flatPreset("easy_npc:humanoid"), PARENT), CHILD, this.presetLoader));
  }

  @Test
  @DisplayName("A preset that references itself is rejected")
  void testSelfReferenceIsRejected() {
    assertNull(
        PresetInheritance.resolve(
            withParent(flatPreset("easy_npc:humanoid"), CHILD), CHILD, this.presetLoader));
  }

  @Test
  @DisplayName("A cycle deeper in the chain is rejected")
  void testCycleBetweenTwoParentsIsRejected() {
    this.presets.put(PARENT, withParent(flatPreset("easy_npc:humanoid"), GRANDPARENT));
    this.presets.put(GRANDPARENT, withParent(flatPreset("easy_npc:humanoid"), PARENT));

    assertNull(
        PresetInheritance.resolve(
            withParent(flatPreset("easy_npc:humanoid"), PARENT), CHILD, this.presetLoader));
  }

  @Test
  @DisplayName("The display name of the parent is not inherited")
  void testDisplayNameIsNotInherited() {
    CompoundTag parentTag = wrappedPreset("easy_npc:humanoid");
    CompoundTag parentMetadata = new CompoundTag();
    parentMetadata.putString(PresetMetadata.TAG_NAME, "Parent Preset");
    parentMetadata.putString(PresetMetadata.TAG_AUTHOR, "Kaworru");
    parentTag.put(PresetDataCapable.PRESET_METADATA_TAG, parentMetadata);
    this.presets.put(PARENT, parentTag);

    CompoundTag resolvedTag =
        PresetInheritance.resolve(
            withParent(wrappedPreset("easy_npc:humanoid"), PARENT), CHILD, this.presetLoader);
    CompoundTag resolvedMetadata = resolvedTag.getCompound(PresetDataCapable.PRESET_METADATA_TAG);

    assertFalse(resolvedMetadata.contains(PresetMetadata.TAG_NAME));
    assertEquals("Kaworru", resolvedMetadata.getString(PresetMetadata.TAG_AUTHOR));
  }

  @Test
  @DisplayName("A rejected chain of parents leaves the preset of the caller untouched")
  void testRejectedChainDoesNotChangeTheInput() {
    CompoundTag childTag = withParent(flatPreset("easy_npc:humanoid"), PARENT);

    assertNull(PresetInheritance.resolve(childTag, CHILD, this.presetLoader));
    assertEquals(PARENT.toString(), childTag.getString(PresetData.PARENT_TAG));
  }

  @Test
  @DisplayName("A chain of exactly the allowed number of parents resolves down to the last one")
  void testDepthLimitIsReached() {
    this.buildParentChain(PresetInheritance.MAX_PARENT_DEPTH);

    CompoundTag resolvedTag = this.resolveChain();

    assertNotNull(resolvedTag);
    assertEquals("reached", resolvedTag.getString(chainTag(PresetInheritance.MAX_PARENT_DEPTH)));
    assertEquals("chain_1", resolvedTag.getString("Team"));
  }

  @Test
  @DisplayName("A chain with one parent too many is rejected")
  void testDepthLimitIsRejected() {
    this.buildParentChain(PresetInheritance.MAX_PARENT_DEPTH + 1);

    assertNull(this.resolveChain());
  }

  @Test
  void testMissingParentIsRejected() {
    assertNull(
        PresetInheritance.resolve(
            withParent(flatPreset("easy_npc:humanoid"), PARENT), CHILD, this.presetLoader));
  }

  @Test
  void testInvalidParentIdIsRejected() {
    CompoundTag childTag = flatPreset("easy_npc:humanoid");
    childTag.putString(PresetData.PARENT_TAG, "Not An Id");

    assertNull(PresetInheritance.resolve(childTag, CHILD, this.presetLoader));
  }

  @Test
  @DisplayName("A parent that stores its entity data differently is not merged blindly")
  void testMixedLayoutIsRejected() {
    this.presets.put(PARENT, wrappedPreset("easy_npc:humanoid"));

    assertNull(
        PresetInheritance.resolve(
            withParent(flatPreset("easy_npc:humanoid"), PARENT), CHILD, this.presetLoader));
  }

  private CompoundTag resolveChain() {
    return PresetInheritance.resolve(
        withParent(flatPreset("easy_npc:humanoid"), chainLocation(1)), CHILD, this.presetLoader);
  }

  private void buildParentChain(int length) {
    for (int index = 1; index <= length; index++) {
      CompoundTag parentTag = flatPreset("easy_npc:humanoid");
      parentTag.putString(chainTag(index), "reached");
      parentTag.putString("Team", "chain_" + index);
      if (index < length) {
        withParent(parentTag, chainLocation(index + 1));
      }
      this.presets.put(chainLocation(index), parentTag);
    }
  }
}
