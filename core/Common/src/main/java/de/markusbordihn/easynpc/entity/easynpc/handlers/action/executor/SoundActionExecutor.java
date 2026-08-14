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

package de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.SoundActionData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.Identifier;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class SoundActionExecutor {

  private static final Logger log = LogManager.getLogger(SoundActionExecutor.class);

  private SoundActionExecutor() {}

  public static boolean play(ActionDataEntry actionDataEntry, EasyNPC<?> easyNPC) {
    if (actionDataEntry == null || easyNPC == null) {
      return false;
    }

    LivingEntity livingEntity = easyNPC.getLivingEntity();
    Level level = easyNPC.getEntityLevel();
    if (livingEntity == null || level == null || level.isClientSide()) {
      return false;
    }

    SoundActionData soundActionData = actionDataEntry.soundActionData();
    Identifier soundLocation = soundActionData.getSoundLocation();
    if (soundLocation == null) {
      log.warn("Skipping sound action with invalid sound id: {}", soundActionData.soundId());
      return false;
    }

    SoundEvent soundEvent =
        BuiltInRegistries.SOUND_EVENT
            .getOptional(soundLocation)
            .orElseGet(() -> SoundEvent.createVariableRangeEvent(soundLocation));
    level.playSound(
        null,
        livingEntity.getX(),
        livingEntity.getY(),
        livingEntity.getZ(),
        soundEvent,
        soundActionData.soundSource(),
        soundActionData.volume(),
        soundActionData.pitch());
    return true;
  }
}
