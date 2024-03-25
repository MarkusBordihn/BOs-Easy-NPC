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

package de.markusbordihn.easynpc.entity;

import de.markusbordihn.easynpc.Constants;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientEntityEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerEntityEvents;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class LivingEntityEventHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private LivingEntityEventHandler() {}

  public static void registerServerEntityEvents() {
    log.info("{} Registering Server Entity Events ...", Constants.LOG_REGISTER_PREFIX);

    ServerEntityEvents.ENTITY_LOAD.register(
        (Entity entity, ServerLevel level) -> {
          if (entity instanceof LivingEntity livingEntity) {
            LivingEntityEvents.handleLivingEntityJoinEvent(livingEntity);
          }
        });

    ServerEntityEvents.ENTITY_UNLOAD.register(
        (Entity entity, ServerLevel level) -> {
          if (entity instanceof LivingEntity livingEntity) {
            LivingEntityEvents.handleLivingEntityLeaveEvent(livingEntity);
          }
        });
  }

  public static void registerClientEntityEvents() {
    log.info("{} Registering Client Entity Events ...", Constants.LOG_REGISTER_PREFIX);

    ClientEntityEvents.ENTITY_LOAD.register(
        (Entity entity, ClientLevel level) -> {
          if (entity instanceof LivingEntity livingEntity) {
            LivingEntityEvents.handleLivingEntityJoinEvent(livingEntity);
          }
        });

    ClientEntityEvents.ENTITY_UNLOAD.register(
        (Entity entity, ClientLevel level) -> {
          if (entity instanceof LivingEntity livingEntity) {
            LivingEntityEvents.handleLivingEntityLeaveEvent(livingEntity);
          }
        });
  }
}
