/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.client;

import de.markusbordihn.easynpc.client.compat.cobblemon.CobblemonVariantHelper;
import de.markusbordihn.easynpc.client.renderer.entity.SpeechBubbleFrameRenderer;
import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.compat.cobblemon.CobblemonLoader;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesLoader;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientLifecycleEvents;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents;
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayConnectionEvents;
import net.fabricmc.fabric.api.client.rendering.v1.WorldRenderContext;
import net.fabricmc.fabric.api.client.rendering.v1.WorldRenderEvents;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ClientPacketListener;
import net.minecraft.client.renderer.MultiBufferSource;

public class ClientEventHandler {

  private ClientEventHandler() {}

  public static void registerClientEvents() {
    ClientLifecycleEvents.CLIENT_STARTED.register(ClientEventHandler::onClientStarted);
    ClientTickEvents.END_CLIENT_TICK.register(ClientEventHandler::onClientTick);
    ClientPlayConnectionEvents.JOIN.register(
        (handler, sender, client) -> {
          if (CompatConstants.MOD_COBBLEMON_LOADED) {
            CobblemonLoader.setFemaleVariantFilter(CobblemonVariantHelper::hasFemaleVariant);
            CobblemonLoader.setAspectProvider(CobblemonVariantHelper::getAvailableAspects);
            CobblemonLoader.register();
          }
          if (CompatConstants.MOD_EASY_MODEL_ENTITIES_LOADED) {
            EasyModelEntitiesLoader.registerClient();
          }
        });
    ClientPlayConnectionEvents.DISCONNECT.register(ClientEventHandler::onDisconnect);
    WorldRenderEvents.AFTER_ENTITIES.register(ClientEventHandler::onAfterEntities);
  }

  public static void onAfterEntities(WorldRenderContext context) {
    Minecraft minecraft = Minecraft.getInstance();
    if (context.world() != minecraft.level
        || !(context.consumers() instanceof MultiBufferSource.BufferSource bufferSource)) {
      return;
    }

    SpeechBubbleFrameRenderer.renderFrame(
        minecraft,
        context.matrixStack(),
        bufferSource,
        context.camera(),
        context.projectionMatrix(),
        context.tickCounter().getGameTimeDeltaPartialTick(false));
  }

  public static void onClientStarted(Minecraft client) {
    ClientEvents.handleClientStartedEvent(client);
  }

  public static void onClientTick(Minecraft client) {
    ClientEvents.handleClientTickEvent();
  }

  public static void onDisconnect(ClientPacketListener handler, Minecraft client) {
    ClientEvents.handleWorldUnloadEvent();
  }
}
