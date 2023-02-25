/**
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

package de.markusbordihn.easynpc.entity;

import java.util.HashSet;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import de.markusbordihn.easynpc.Constants;

@Mod.EventBusSubscriber()
public class EasyNPCEntityEvents {

  public static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final Set<EasyNPCEntity> changeInvulnerableTimeSet = new HashSet<>();

  protected EasyNPCEntityEvents() {}

  @SubscribeEvent(priority = EventPriority.HIGHEST)
  public static void handleEntityInteractHIGHEST(PlayerInteractEvent.EntityInteract event) {
    if (event.isCanceled()) {
      return;
    }

    if (event.getTarget() instanceof EasyNPCEntity easyNPCEntity) {
      log.debug("HIGHEST Entity {} was right clicked by {}", easyNPCEntity, event.getPlayer());

      // Change invulnerable time to 1 tick, if not already greater than 0 and store the
      // easyNPCEntity into changeInvulnerableTimeSet.
      if (easyNPCEntity.invulnerableTime == 0) {
        easyNPCEntity.invulnerableTime = 1;
        changeInvulnerableTimeSet.add(easyNPCEntity);
      }
    }
  }

  @SubscribeEvent(priority = EventPriority.LOWEST)
  public static void handleEntityInteractLOWEST(PlayerInteractEvent.EntityInteract event) {
    if (event.isCanceled()) {
      return;
    }

    if (event.getTarget() instanceof EasyNPCEntity easyNPCEntity) {
      log.debug("LOWEST Entity {} was right clicked by {}", easyNPCEntity, event.getPlayer());

      // Change invulnerable time back to 0, if it was changed before and remove the easyNPCEntity
      // from changeInvulnerableTimeSet.
      if (changeInvulnerableTimeSet.contains(easyNPCEntity)) {
        easyNPCEntity.invulnerableTime = 0;
        changeInvulnerableTimeSet.remove(easyNPCEntity);
      }
    }
  }

}
