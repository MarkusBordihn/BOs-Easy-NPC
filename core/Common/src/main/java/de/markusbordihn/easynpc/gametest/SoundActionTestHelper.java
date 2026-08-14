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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.SoundActionData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.SoundActionExecutor;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class SoundActionTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final String KNOWN_SOUND_ID = "minecraft:entity.villager.yes";
  private static final String UNKNOWN_SOUND_ID = "easy_npc:gametest.unknown_sound";
  private static final String MALFORMED_SOUND_ID = "not a sound id";

  private SoundActionTestHelper() {}

  private static ActionDataEntry soundAction(String soundId) {
    return new ActionDataEntry(ActionDataType.SOUND)
        .withSoundActionData(new SoundActionData(soundId));
  }

  public static void assertSoundActionPlaysAKnownSound(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    GameTestHelpers.assertTrue(
        helper,
        "A known sound must be played",
        SoundActionExecutor.play(soundAction(KNOWN_SOUND_ID), easyNPC));
    GameTestHelpers.assertTrue(
        helper,
        "A sound of another mod or resource pack must be played",
        SoundActionExecutor.play(soundAction(UNKNOWN_SOUND_ID), easyNPC));
  }

  public static void assertSoundActionKeepsItsSourceVolumeAndPitch(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ActionDataEntry actionDataEntry =
        new ActionDataEntry(ActionDataType.SOUND)
            .withSoundActionData(
                new SoundActionData(KNOWN_SOUND_ID, SoundSource.AMBIENT, 42.0F, 4.0F));

    GameTestHelpers.assertEquals(
        helper,
        "A sound volume above the limit must be capped",
        SoundActionData.MAX_VOLUME,
        actionDataEntry.soundActionData().volume());
    GameTestHelpers.assertEquals(
        helper,
        "A sound pitch above the limit must be capped",
        SoundActionData.MAX_PITCH,
        actionDataEntry.soundActionData().pitch());
    GameTestHelpers.assertEquals(
        helper,
        "The chosen sound source must be kept",
        SoundSource.AMBIENT,
        actionDataEntry.soundActionData().soundSource());
    GameTestHelpers.assertTrue(
        helper,
        "A sound with its own source, volume and pitch must be played",
        SoundActionExecutor.play(actionDataEntry, easyNPC));
  }

  public static void assertSoundActionWithoutAValidSoundIsSkipped(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    GameTestHelpers.assertTrue(
        helper,
        "A sound action without a sound must be skipped",
        !SoundActionExecutor.play(soundAction(""), easyNPC));
    GameTestHelpers.assertTrue(
        helper,
        "A sound action with a malformed sound id must be skipped",
        !SoundActionExecutor.play(soundAction(MALFORMED_SOUND_ID), easyNPC));
    GameTestHelpers.assertTrue(
        helper,
        "A sound action without an NPC must be skipped",
        !SoundActionExecutor.play(soundAction(KNOWN_SOUND_ID), null));
    GameTestHelpers.assertTrue(
        helper, "A missing sound action must be skipped", !SoundActionExecutor.play(null, easyNPC));
  }
}
