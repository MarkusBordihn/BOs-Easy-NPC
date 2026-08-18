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

package de.markusbordihn.easynpc.data.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class MoveActionDataTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  @Test
  @DisplayName("Values outside of the allowed range are clamped")
  void testValuesAreClamped() {
    MoveActionData tooSmall = new MoveActionData(MoveTargetType.POSITION, 0.0, 0.0F, 0, false);
    assertEquals(MoveActionData.MIN_SPEED_MODIFIER, tooSmall.speedModifier());
    assertEquals(MoveActionData.MIN_ARRIVAL_RADIUS, tooSmall.arrivalRadius());
    assertEquals(MoveActionData.MIN_TIMEOUT_TICKS, tooSmall.timeoutTicks());

    MoveActionData tooLarge =
        new MoveActionData(MoveTargetType.POSITION, 99.0, 99.0F, 99999, false);
    assertEquals(MoveActionData.MAX_SPEED_MODIFIER, tooLarge.speedModifier());
    assertEquals(MoveActionData.MAX_ARRIVAL_RADIUS, tooLarge.arrivalRadius());
    assertEquals(MoveActionData.MAX_TIMEOUT_TICKS, tooLarge.timeoutTicks());
  }

  @Test
  @DisplayName("A missing target type falls back to the position target")
  void testMissingTargetTypeFallsBack() {
    assertEquals(
        MoveTargetType.POSITION, new MoveActionData(null, 1.0, 2.0F, 200, false).targetType());
    assertEquals(MoveTargetType.POSITION, MoveActionData.fromTag(new CompoundTag()).targetType());
    assertEquals(MoveActionData.DEFAULT, MoveActionData.fromTag(null));
  }

  @Test
  @DisplayName("A default move action writes an empty tag")
  void testDefaultWritesAnEmptyTag() {
    assertTrue(MoveActionData.DEFAULT.createTag().isEmpty());
    assertEquals(
        MoveActionData.DEFAULT, MoveActionData.fromTag(MoveActionData.DEFAULT.createTag()));
  }

  @Test
  @DisplayName("Only values differing from the default are written")
  void testOnlyChangedValuesAreWritten() {
    CompoundTag compoundTag = MoveActionData.DEFAULT.withArrivalRadius(4.5F).createTag();
    assertEquals(1, compoundTag.size());
    assertTrue(compoundTag.contains(MoveActionData.DATA_RADIUS_TAG));
    assertFalse(compoundTag.contains(MoveActionData.DATA_TARGET_TYPE_TAG));
    assertFalse(compoundTag.contains(MoveActionData.DATA_TELEPORT_TAG));
  }

  @Test
  @DisplayName("Every value survives a tag round trip")
  void testTagRoundTrip() {
    MoveActionData moveActionData = new MoveActionData(MoveTargetType.HOME, 1.75, 3.25F, 640, true);
    assertEquals(moveActionData, MoveActionData.fromTag(moveActionData.createTag()));
  }

  @Test
  @DisplayName("Only position based targets need a block position")
  void testResolvableTarget() {
    MoveActionData positionTarget = new MoveActionData(MoveTargetType.POSITION);
    assertFalse(positionTarget.hasResolvableTarget(null));
    assertFalse(positionTarget.hasResolvableTarget(BlockPos.ZERO));
    assertTrue(positionTarget.hasResolvableTarget(new BlockPos(8, 64, 8)));

    assertFalse(new MoveActionData(MoveTargetType.RELATIVE).hasResolvableTarget(BlockPos.ZERO));
    assertTrue(new MoveActionData(MoveTargetType.INITIATOR).hasResolvableTarget(BlockPos.ZERO));
    assertTrue(new MoveActionData(MoveTargetType.OWNER).hasResolvableTarget(null));
    assertTrue(new MoveActionData(MoveTargetType.HOME).hasResolvableTarget(null));
  }
}
