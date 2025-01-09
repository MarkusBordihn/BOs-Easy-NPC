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

package de.markusbordihn.easynpc.server;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.backup.BackupManager;
import de.markusbordihn.easynpc.io.DataFileHandler;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.level.storage.LevelResource;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ServerEvents {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ServerEvents() {}

  public static void handleServerStarting(MinecraftServer minecraftServer) {
    if (minecraftServer == null) {
      return;
    }

    log.info("{} Server is starting Events ...", Constants.LOG_REGISTER_PREFIX);

    // Set world directory for server.
    Constants.WORLD_DIR = minecraftServer.getWorldPath(LevelResource.ROOT);

    // Prepare custom data directory for server.
    DataFileHandler.registerServerDataFiles(minecraftServer);
  }

  public static void handleServerTick(MinecraftServer minecraftServer) {
    if (minecraftServer == null) {
      return;
    }

    // Perform backup each hour.
    BackupManager.performBackup();
  }
}
