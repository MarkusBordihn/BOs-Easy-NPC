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

package de.markusbordihn.easynpc.utils;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.scale.CustomScale;
import java.util.Set;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.resources.ResourceLocation;
import org.junit.jupiter.api.Test;

class CompoundTagUtilsTest {

  @Test
  void testBlockPos_roundTrip_positive() {
    BlockPos original = new BlockPos(10, 64, 200);
    CompoundTag tag = CompoundTagUtils.writeBlockPos(original);
    BlockPos result = CompoundTagUtils.readBlockPos(tag);
    assertEquals(original, result);
  }

  @Test
  void testBlockPos_roundTrip_negative() {
    BlockPos original = new BlockPos(-100, -5, -300);
    CompoundTag tag = CompoundTagUtils.writeBlockPos(original);
    BlockPos result = CompoundTagUtils.readBlockPos(tag);
    assertEquals(original, result);
  }

  @Test
  void testBlockPos_roundTrip_zero() {
    BlockPos original = new BlockPos(0, 0, 0);
    CompoundTag tag = CompoundTagUtils.writeBlockPos(original);
    BlockPos result = CompoundTagUtils.readBlockPos(tag);
    assertEquals(BlockPos.ZERO, result);
  }

  @Test
  void testReadBlockPos_null_returnsZero() {
    assertEquals(BlockPos.ZERO, CompoundTagUtils.readBlockPos(null));
  }

  @Test
  void testReadBlockPos_missingTags_returnsZero() {
    assertEquals(BlockPos.ZERO, CompoundTagUtils.readBlockPos(new CompoundTag()));
  }

  @Test
  void testReadBlockPos_partialTags_returnsZero() {
    CompoundTag partial = new CompoundTag();
    partial.putInt("X", 5);
    assertEquals(BlockPos.ZERO, CompoundTagUtils.readBlockPos(partial));
  }

  @Test
  void testScale_roundTrip() {
    CompoundTag tag = CompoundTagUtils.writeScale(1.5f, 2.0f, 0.75f);
    CustomScale result = CompoundTagUtils.readCustomScale(tag);
    assertNotNull(result);
    assertEquals(1.5f, result.x(), 0.001f);
    assertEquals(2.0f, result.y(), 0.001f);
    assertEquals(0.75f, result.z(), 0.001f);
  }

  @Test
  void testScale_roundTrip_zeroAndNegative() {
    CompoundTag tag = CompoundTagUtils.writeScale(0.0f, -1.0f, 0.5f);
    CustomScale result = CompoundTagUtils.readCustomScale(tag);
    assertNotNull(result);
    assertEquals(0.0f, result.x(), 0.001f);
    assertEquals(-1.0f, result.y(), 0.001f);
    assertEquals(0.5f, result.z(), 0.001f);
  }

  @Test
  void testReadCustomScale_null_returnsNull() {
    assertNull(CompoundTagUtils.readCustomScale(null));
  }

  @Test
  void testCustomScale_roundTrip() {
    CustomScale original = new CustomScale(1.0f, 2.5f, 0.3f);
    CompoundTag tag = CompoundTagUtils.writeCustomScale(original);
    CustomScale result = CompoundTagUtils.readCustomScale(tag);
    assertNotNull(result);
    assertEquals(original.x(), result.x(), 0.001f);
    assertEquals(original.y(), result.y(), 0.001f);
    assertEquals(original.z(), result.z(), 0.001f);
  }

  @Test
  void testResourceLocations_roundTrip_singleEntry() {
    Set<ResourceLocation> original = Set.of(new ResourceLocation("minecraft", "stone"));
    ListTag tag = CompoundTagUtils.writeResourceLocations(original);
    Set<ResourceLocation> result = CompoundTagUtils.readResourceLocations(tag);
    assertEquals(original, result);
  }

  @Test
  void testResourceLocations_roundTrip_multipleEntries() {
    Set<ResourceLocation> original =
        Set.of(
            new ResourceLocation("minecraft", "stone"),
            new ResourceLocation("easynpc", "custom_npc"),
            new ResourceLocation("minecraft", "dirt"));
    ListTag tag = CompoundTagUtils.writeResourceLocations(original);
    Set<ResourceLocation> result = CompoundTagUtils.readResourceLocations(tag);
    assertEquals(original, result);
  }

  @Test
  void testResourceLocations_emptySet() {
    Set<ResourceLocation> original = Set.of();
    ListTag tag = CompoundTagUtils.writeResourceLocations(original);
    Set<ResourceLocation> result = CompoundTagUtils.readResourceLocations(tag);
    assertTrue(result.isEmpty());
  }

  @Test
  void testPutIfNotEmpty_skipsEmptyChild() {
    CompoundTag parent = new CompoundTag();

    CompoundTagUtils.putIfNotEmpty(parent, "Child", new CompoundTag());

    assertFalse(parent.contains("Child"));
  }

  @Test
  void testPutIfNotEmpty_writesNonEmptyChild() {
    CompoundTag parent = new CompoundTag();
    CompoundTag child = new CompoundTag();
    child.putString("Value", "test");

    CompoundTagUtils.putIfNotEmpty(parent, "Child", child);

    assertTrue(parent.contains("Child"));
    assertEquals("test", parent.getCompound("Child").getString("Value"));
  }
}
