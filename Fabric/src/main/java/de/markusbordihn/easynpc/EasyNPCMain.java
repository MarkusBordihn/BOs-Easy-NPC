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

import de.markusbordihn.easynpc.block.ModBlocks;
import de.markusbordihn.easynpc.commands.ModArgumentTypes;
import de.markusbordihn.easynpc.commands.manager.CommandManager;
import de.markusbordihn.easynpc.compat.CompatHandler;
import de.markusbordihn.easynpc.compat.CompatManager;
import de.markusbordihn.easynpc.config.Config;
import de.markusbordihn.easynpc.debug.DebugManager;
import de.markusbordihn.easynpc.entity.LivingEntityEventHandler;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.item.ModItems;
import de.markusbordihn.easynpc.menu.MenuHandler;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import de.markusbordihn.easynpc.network.ClientNetworkMessageHandler;
import de.markusbordihn.easynpc.network.NetworkHandler;
import de.markusbordihn.easynpc.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.network.NetworkHandlerManagerType;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import de.markusbordihn.easynpc.server.ServerEvents;
import net.fabricmc.api.ModInitializer;
import net.fabricmc.fabric.api.command.v2.CommandRegistrationCallback;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.fabricmc.loader.api.FabricLoader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCMain implements ModInitializer {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  @Override
  public void onInitialize() {
    log.info("Initializing {} (Fabric) ...", Constants.MOD_NAME);

    log.info("{} Debug Manager ...", Constants.LOG_REGISTER_PREFIX);
    if (System.getProperty("fabric.development") != null) {
      DebugManager.setDevelopmentEnvironment(true);
    }
    DebugManager.checkForDebugLogging(Constants.LOG_NAME);

    log.info("{} Constants ...", Constants.LOG_REGISTER_PREFIX);
    Constants.GAME_DIR = FabricLoader.getInstance().getGameDir();
    Constants.CONFIG_DIR = FabricLoader.getInstance().getConfigDir();

    log.info("{} Configuration ...", Constants.LOG_REGISTER_PREFIX);
    Config.register();

    log.info("{} Entity Data Serializers ...", Constants.LOG_REGISTER_PREFIX);
    EntityDataSerializersManager.register();

    log.info("{} Compatibility Handler ...", Constants.LOG_REGISTER_PREFIX);
    CompatManager.registerCompatHandler(new CompatHandler());

    log.info("{} Entity Types ...", Constants.LOG_REGISTER_PREFIX);
    ModEntityType.registerEntitiesAttributes();

    log.info("{} Blocks ...", Constants.LOG_REGISTER_PREFIX);
    ModBlocks.registerModBlocks();

    log.info("{} Blocks Entities ...", Constants.LOG_REGISTER_PREFIX);
    ModBlocks.registerModBlockEntities();

    log.info("{} Items ...", Constants.LOG_REGISTER_PREFIX);
    ModItems.registerModItems();

    log.info("{} Command register event ...", Constants.LOG_REGISTER_PREFIX);
    CommandRegistrationCallback.EVENT.register(
        (dispatcher, commandBuildContext, commandSelection) ->
            CommandManager.registerCommands(dispatcher, commandBuildContext));

    log.info("{} Server Events ...", Constants.LOG_REGISTER_PREFIX);
    ServerLifecycleEvents.SERVER_STARTING.register(ServerEvents::handleServerStarting);
    LivingEntityEventHandler.registerServerEntityEvents();

    log.info("{} Menu Handler ...", Constants.LOG_REGISTER_PREFIX);
    MenuManager.registerMenuHandler(new MenuHandler());

    log.info("{} Menu Types ...", Constants.LOG_REGISTER_PREFIX);
    ModMenuTypes.register();

    log.info("{} Network Handler ...", Constants.LOG_REGISTER_PREFIX);
    NetworkHandlerManager.registerHandler(new NetworkHandler());
    NetworkHandlerManager.registerNetworkMessages(NetworkHandlerManagerType.SERVER);
    NetworkMessageHandlerManager.registerClientHandler(new ClientNetworkMessageHandler());

    log.info("{} Argument Types ...", Constants.LOG_REGISTER_PREFIX);
    ModArgumentTypes.register();
  }
}
