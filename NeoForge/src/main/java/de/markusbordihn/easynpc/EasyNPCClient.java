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

package de.markusbordihn.easynpc;

import de.markusbordihn.easynpc.client.model.ModModelLayer;
import de.markusbordihn.easynpc.client.renderer.ClientRenderer;
import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.client.screen.ClientScreens;
import de.markusbordihn.easynpc.io.DataFileHandler;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.network.ServerNetworkMessageHandler;
import de.markusbordihn.easynpc.tabs.ModTabs;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.event.lifecycle.FMLClientSetupEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod(value = Constants.MOD_ID, dist = Dist.CLIENT)
public class EasyNPCClient {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public EasyNPCClient(IEventBus modEventBus, ModContainer modContainer) {
    log.info("{} Client events ...", Constants.LOG_REGISTER_PREFIX);
    modEventBus.addListener(ModModelLayer::registerEntityLayerDefinitions);
    modEventBus.addListener(ClientRenderer::registerEntityRenderers);
    modEventBus.addListener(ClientScreens::registerScreens);
    modEventBus.addListener(
        (final FMLClientSetupEvent event) -> {
          log.info("{} Register Data Files ...", Constants.LOG_REGISTER_PREFIX);
          event.enqueueWork(DataFileHandler::registerDataFiles);

          log.info("{} Register Entity Type Manager ...", Constants.LOG_REGISTER_PREFIX);
          event.enqueueWork(EntityTypeManager::register);
        });
    NetworkMessageHandlerManager.registerServerHandler(new ServerNetworkMessageHandler());
    ModTabs.CREATIVE_TABS.register(modEventBus);
  }
}
