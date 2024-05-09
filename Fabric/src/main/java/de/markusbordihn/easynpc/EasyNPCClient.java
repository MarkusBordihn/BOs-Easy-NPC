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
import de.markusbordihn.easynpc.client.screen.ClientScreens;
import de.markusbordihn.easynpc.entity.LivingEntityEventHandler;
import de.markusbordihn.easynpc.io.DataFileHandler;
import de.markusbordihn.easynpc.network.NetworkHandler;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.network.ServerNetworkMessageHandler;
import de.markusbordihn.easynpc.tabs.ModTabs;
import net.fabricmc.api.ClientModInitializer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCClient implements ClientModInitializer {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  @Override
  public void onInitializeClient() {
    log.info("Initializing {} (Fabric-Client) ...", Constants.MOD_NAME);

    log.info("{} Entity Renders ...", Constants.LOG_REGISTER_PREFIX);
    ClientRenderer.registerEntityRenderers();

    log.info("{} Entity Layer Definitions ...", Constants.LOG_REGISTER_PREFIX);
    ModModelLayer.registerEntityLayerDefinitions();

    log.info("{} Entity Client Events ...", Constants.LOG_REGISTER_PREFIX);
    LivingEntityEventHandler.registerClientEntityEvents();

    log.info("{} Tabs ...", Constants.LOG_REGISTER_PREFIX);
    ModTabs.registerModTabs();

    log.info("{} Register Data Files ...", Constants.LOG_REGISTER_PREFIX);
    DataFileHandler.registerDataFiles();

    log.info("{} Client Network Handler ...", Constants.LOG_REGISTER_PREFIX);
    NetworkHandler.registerClientNetworkHandler();
    NetworkMessageHandlerManager.registerServerHandler(new ServerNetworkMessageHandler());

    log.info("{} Client Screens ...", Constants.LOG_REGISTER_PREFIX);
    ClientScreens.registerScreens();
  }
}
