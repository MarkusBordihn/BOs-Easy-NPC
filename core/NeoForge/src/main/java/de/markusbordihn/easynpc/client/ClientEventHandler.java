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
import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.compat.cobblemon.CobblemonLoader;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesLoader;
import net.minecraft.client.Minecraft;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.fml.event.lifecycle.FMLClientSetupEvent;
import net.neoforged.neoforge.client.event.ClientTickEvent;
import net.neoforged.neoforge.event.TagsUpdatedEvent;

@EventBusSubscriber(value = Dist.CLIENT)
public class ClientEventHandler {

  @SubscribeEvent
  public static void onClientSetup(FMLClientSetupEvent event) {
    event.enqueueWork(() -> ClientEvents.handleClientStartedEvent(Minecraft.getInstance()));
  }
}

@EventBusSubscriber(value = Dist.CLIENT)
class ClientGameEventHandler {

  @SubscribeEvent
  public static void onClientTick(ClientTickEvent.Post event) {
    ClientEvents.handleClientTickEvent();
  }

  @SubscribeEvent
  public static void onTagsUpdated(TagsUpdatedEvent event) {
    if (CompatConstants.MOD_COBBLEMON_LOADED) {
      CobblemonLoader.setFemaleVariantFilter(CobblemonVariantHelper::hasFemaleVariant);
      CobblemonLoader.setAspectProvider(CobblemonVariantHelper::getAvailableAspects);
      CobblemonLoader.register();
    }
    if (CompatConstants.MOD_EASY_MODEL_ENTITIES_LOADED) {
      EasyModelEntitiesLoader.register();
    }
  }
}
