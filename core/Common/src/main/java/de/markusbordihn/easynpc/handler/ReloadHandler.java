/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.PathfinderMob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ReloadHandler {

  public static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ReloadHandler() {}

  public static boolean reloadNPC(EasyNPC<?> easyNPC, ServerLevel serverLevel) {
    if (easyNPC == null || serverLevel == null) {
      log.error(
          "Unable to reload NPC with invalid EasyNPC {} or ServerLevel {}!", easyNPC, serverLevel);
      return false;
    }

    PathfinderMob entity = easyNPC.getPathfinderMob();
    if (entity == null || !entity.isAlive()) {
      log.warn(
          "Unable to reload NPC {} because entity is null or not alive!", easyNPC.getEntityUUID());
      return false;
    }

    // Save entity and entity type
    CompoundTag compoundTag = easyNPC.getEntity().saveWithoutId(new CompoundTag());

    // Reload entity data
    log.debug("Reloading NPC {} at position {}", easyNPC.getEntityUUID(), entity.position());
    entity.load(compoundTag);

    // Force update visibility for all players
    entity.refreshDimensions();
    return true;
  }
}
