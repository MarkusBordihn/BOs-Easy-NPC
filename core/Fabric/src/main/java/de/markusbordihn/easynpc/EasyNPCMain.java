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
import de.markusbordihn.easynpc.debug.Logger;
import de.markusbordihn.easynpc.entity.LivingEntityEventHandler;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.io.DataFileHandler;
import de.markusbordihn.easynpc.item.ModItems;
import de.markusbordihn.easynpc.menu.MenuHandler;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import de.markusbordihn.easynpc.network.*;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import de.markusbordihn.easynpc.server.ServerEvents;
import net.fabricmc.api.EnvType;
import net.fabricmc.api.ModInitializer;
import net.fabricmc.fabric.api.command.v2.CommandRegistrationCallback;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerTickEvents;
import net.fabricmc.loader.api.FabricLoader;

public class EasyNPCMain implements ModInitializer {

  @Override
  public void onInitialize() {
    Logger.INSTANCE.info("Initializing {} (Fabric) ...", Constants.MOD_NAME);

    Logger.INSTANCE.info("{} Debug Manager ...", Constants.LOG_REGISTER_PREFIX);
    if (System.getProperty("fabric.development") != null) {
      DebugManager.setDevelopmentEnvironment(true);
    }
    DebugManager.checkForDebugLogging(Constants.LOG_NAME);

    Logger.INSTANCE.info("{} Constants ...", Constants.LOG_REGISTER_PREFIX);
    Constants.GAME_DIR = FabricLoader.getInstance().getGameDir();
    Constants.CONFIG_DIR = FabricLoader.getInstance().getConfigDir();

    Logger.INSTANCE.info("{} Configuration ...", Constants.LOG_REGISTER_PREFIX);
    Config.register(FabricLoader.getInstance().getEnvironmentType() == EnvType.SERVER);

    Logger.INSTANCE.info("{} Common Data Files ...", Constants.LOG_REGISTER_PREFIX);
    DataFileHandler.registerCommonDataFiles();

    Logger.INSTANCE.info("{} Entity Data Serializers ...", Constants.LOG_REGISTER_PREFIX);
    EntityDataSerializersManager.register();

    Logger.INSTANCE.info("{} Compatibility Handler ...", Constants.LOG_REGISTER_PREFIX);
    CompatManager.registerCompatHandler(new CompatHandler());

    Logger.INSTANCE.info("{} Entity Types ...", Constants.LOG_REGISTER_PREFIX);
    ModEntityType.registerEntitiesAttributes();

    Logger.INSTANCE.info("{} Blocks ...", Constants.LOG_REGISTER_PREFIX);
    ModBlocks.registerModBlocks();

    Logger.INSTANCE.info("{} Blocks Entities ...", Constants.LOG_REGISTER_PREFIX);
    ModBlocks.registerModBlockEntities();

    Logger.INSTANCE.info("{} Items ...", Constants.LOG_REGISTER_PREFIX);
    ModItems.registerModItems();

    Logger.INSTANCE.info("{} Command register event ...", Constants.LOG_REGISTER_PREFIX);
    CommandRegistrationCallback.EVENT.register(
        (dispatcher, commandBuildContext, commandSelection) ->
            CommandManager.registerCommands(dispatcher, commandBuildContext));

    Logger.INSTANCE.info("{} Server Events ...", Constants.LOG_REGISTER_PREFIX);
    ServerLifecycleEvents.SERVER_STARTING.register(ServerEvents::handleServerStarting);
    ServerTickEvents.END_SERVER_TICK.register(ServerEvents::handleServerTick);
    LivingEntityEventHandler.registerServerEntityEvents();

    Logger.INSTANCE.info("{} Menu Handler ...", Constants.LOG_REGISTER_PREFIX);
    MenuManager.registerMenuHandler(new MenuHandler());

    Logger.INSTANCE.info("{} Menu Types ...", Constants.LOG_REGISTER_PREFIX);
    ModMenuTypes.register();

    Logger.INSTANCE.info("{} Server Network Handler ...", Constants.LOG_REGISTER_PREFIX);
    NetworkHandlerManager.registerHandler(new NetworkHandler());
    NetworkHandlerManager.registerNetworkMessages(NetworkHandlerManagerType.SERVER);
    NetworkMessageHandlerManager.registerClientHandler(new ClientNetworkMessageHandler());

    Logger.INSTANCE.info("{} Argument Types ...", Constants.LOG_REGISTER_PREFIX);
    ModArgumentTypes.register();
  }
}
