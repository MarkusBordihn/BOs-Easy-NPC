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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import net.minecraft.world.entity.Mob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PauseManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static boolean globalPause = false;

  private PauseManager() {}

  public static boolean isPaused(EasyNPC<?> easyNPC) {
    if (globalPause) {
      return true;
    }

    StatusDataCapable<?> statusData = easyNPC != null ? easyNPC.getEasyNPCStatusData() : null;
    return statusData != null && statusData.getStatusDataFlag(StatusDataType.PAUSED);
  }

  public static boolean setPaused(EasyNPC<?> easyNPC, boolean paused) {
    StatusDataCapable<?> statusData = easyNPC != null ? easyNPC.getEasyNPCStatusData() : null;
    if (statusData == null) {
      return false;
    }

    statusData.setStatusDataFlag(StatusDataType.PAUSED, paused);
    statusData.markNPCDataUpdated();
    applyNoAi(easyNPC, paused);
    return true;
  }

  public static boolean isGlobalPause() {
    return globalPause;
  }

  public static void setGlobalPause(boolean paused) {
    if (globalPause == paused) {
      return;
    }

    globalPause = paused;
    log.info("{} all Easy NPCs.", paused ? "Paused" : "Resumed");
    LivingEntityManager.getServerEasyNPCEntities()
        .forEach(easyNPC -> applyNoAi(easyNPC, paused || isPaused(easyNPC)));
  }

  public static void reset() {
    globalPause = false;
  }

  public static void enforcePause(EasyNPC<?> easyNPC) {
    applyNoAi(easyNPC, true);
  }

  private static void applyNoAi(EasyNPC<?> easyNPC, boolean paused) {
    Mob mob = easyNPC != null ? easyNPC.getMob() : null;
    if (mob == null || mob.level().isClientSide() || mob.isNoAi() == paused) {
      return;
    }

    if (!paused) {
      log.debug("Clearing NoAI of {} while resuming it, a manually set NoAI is lost.", easyNPC);
    }
    mob.setNoAi(paused);
  }
}
