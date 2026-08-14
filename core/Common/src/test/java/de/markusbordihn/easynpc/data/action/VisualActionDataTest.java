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

import de.markusbordihn.easynpc.data.model.ModelAnimationPlayback;
import de.markusbordihn.easynpc.data.model.ModelAnimationPlaybackMode;
import de.markusbordihn.easynpc.data.model.ModelAnimationSwitchTiming;
import de.markusbordihn.easynpc.data.model.ModelAnimationTransition;
import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.Test;

class VisualActionDataTest {

  @Test
  void storesPoseAsSingleCompactId() {
    ActionDataEntry entry =
        new ActionDataEntry(ActionDataType.SET_POSE).withPoseId("minecraft:crouching");

    CompoundTag tag = entry.createTag();
    assertEquals("minecraft:crouching", tag.getStringOr(ActionDataEntry.DATA_POSE_TAG, ""));
    assertFalse(tag.contains(ActionDataEntry.DATA_COMMAND_TAG));
    assertEquals(entry.poseId(), new ActionDataEntry(tag).poseId());
  }

  @Test
  void omitsDefaultAnimationOptions() {
    ActionDataEntry entry =
        new ActionDataEntry(ActionDataType.PLAY_ANIMATION)
            .withModelAnimationActionData(new ModelAnimationActionData("named:wave"));

    CompoundTag animationTag =
        entry.createTag().getCompoundOrEmpty(ActionDataEntry.DATA_ANIMATION_TAG);
    assertEquals(
        "named:wave", animationTag.getStringOr(ModelAnimationActionData.DATA_NAME_TAG, ""));
    assertFalse(animationTag.contains(ModelAnimationActionData.DATA_LOOP_TAG));
    assertFalse(animationTag.contains(ModelAnimationActionData.DATA_AFTER_CURRENT_TAG));
    assertFalse(animationTag.contains(ModelAnimationActionData.DATA_BLEND_TAG));
  }

  @Test
  void roundTripsNonDefaultAnimationOptions() {
    ModelAnimationActionData animationData =
        new ModelAnimationActionData(
            "idle",
            ModelAnimationPlayback.of(ModelAnimationPlaybackMode.LOOP),
            new ModelAnimationTransition(ModelAnimationSwitchTiming.AFTER_CURRENT, 10.0F));
    ActionDataEntry entry =
        new ActionDataEntry(ActionDataType.PLAY_ANIMATION)
            .withModelAnimationActionData(animationData);

    ActionDataEntry loaded = new ActionDataEntry(entry.createTag());
    assertEquals(animationData, loaded.modelAnimationActionData());
    assertTrue(loaded.isValidAndNotEmpty());
  }

  @Test
  void stopAndRestartNeedNoPayload() {
    CompoundTag stop = new ActionDataEntry(ActionDataType.STOP_ANIMATION).createTag();
    CompoundTag restart = new ActionDataEntry(ActionDataType.RESTART_ANIMATION).createTag();

    assertFalse(stop.contains(ActionDataEntry.DATA_ANIMATION_TAG));
    assertFalse(restart.contains(ActionDataEntry.DATA_ANIMATION_TAG));
    assertTrue(new ActionDataEntry(stop).isValidAndNotEmpty());
    assertTrue(new ActionDataEntry(restart).isValidAndNotEmpty());
  }
}
