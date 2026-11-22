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

package de.markusbordihn.easynpc.io;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.config.CustomSkinConfig;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.utils.PngCompressionUtils;
import de.markusbordihn.easynpc.utils.SkinFileNameUtils;
import de.markusbordihn.easynpc.validator.PngFileValidator;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.Collection;
import java.util.EnumMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ServerSkinIndex {

  public static final int MAX_SKIN_FILE_SIZE = 768 * 1024;
  static final int MAX_SKIN_COUNT = 512;
  static final long MAX_TOTAL_SIZE = 32L * 1024 * 1024;
  static final int MAX_DISPLAY_NAME_LENGTH = 64;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Server Skin Index]";
  private static final int MAX_FOLDER_DEPTH = 5;
  private static final Pattern UNSAFE_DISPLAY_NAME_CHARACTERS =
      Pattern.compile("§.?|[\\p{Cc}\\p{Cf}]");

  private static volatile Map<SkinModel, Map<UUID, ServerSkin>> serverSkins = Map.of();

  private ServerSkinIndex() {}

  public static void reload() {
    Path customDataFolder = DataFileHandler.getCustomDataFolder();
    if (!CustomSkinConfig.SHARE_SERVER_SKINS || customDataFolder == null) {
      clear();
      return;
    }

    load(customDataFolder.resolve(CustomSkinDataFiles.DATA_FOLDER_NAME));
  }

  static void load(Path skinDataFolder) {
    serverSkins = loadServerSkins(skinDataFolder);
  }

  public static void clear() {
    serverSkins = Map.of();
  }

  public static ServerSkin getSkin(SkinModel skinModel, UUID skinUUID) {
    return serverSkins.getOrDefault(skinModel, Map.of()).get(skinUUID);
  }

  public static Collection<ServerSkin> getSkins(SkinModel skinModel) {
    return serverSkins.getOrDefault(skinModel, Map.of()).values();
  }

  public static List<ServerSkin> findSkins(String skinName) {
    List<ServerSkin> matchingSkins =
        findSkins(serverSkin ->
            serverSkin.displayName().equals(skinName)
                || serverSkin.skinUUID().toString().equals(skinName));
    return matchingSkins.isEmpty()
        ? findSkins(serverSkin -> serverSkin.displayName().equalsIgnoreCase(skinName))
        : matchingSkins;
  }

  private static List<ServerSkin> findSkins(Predicate<ServerSkin> skinFilter) {
    return serverSkins.values().stream()
        .flatMap(skinModelSkins -> skinModelSkins.values().stream())
        .filter(skinFilter)
        .toList();
  }

  private static Map<SkinModel, Map<UUID, ServerSkin>> loadServerSkins(Path skinDataFolder) {
    Map<SkinModel, Map<UUID, ServerSkin>> loadedSkins = new EnumMap<>(SkinModel.class);
    int skinCount = 0;
    long totalSize = 0;
    for (SkinModel skinModel : SkinModel.values()) {
      Map<UUID, ServerSkin> skinModelSkins = new LinkedHashMap<>();
      for (Path skinFile : findSkinFiles(skinDataFolder.resolve(skinModel.getName()))) {
        ServerSkin serverSkin = readServerSkin(skinModel, skinFile);
        if (serverSkin == null) {
          continue;
        }

        if (skinCount >= MAX_SKIN_COUNT
            || totalSize + serverSkin.pngBytes().length > MAX_TOTAL_SIZE) {
          log.warn(
              "{} Ignoring {} and all further skins, the limit of {} skins or {} bytes is reached.",
              LOG_PREFIX,
              skinFile,
              MAX_SKIN_COUNT,
              MAX_TOTAL_SIZE);
          loadedSkins.put(skinModel, skinModelSkins);
          return loadedSkins;
        }

        if (skinModelSkins.putIfAbsent(serverSkin.skinUUID(), serverSkin) != null) {
          log.warn("{} Ignoring {} with an already used skin name.", LOG_PREFIX, skinFile);
          continue;
        }

        skinCount++;
        totalSize += serverSkin.pngBytes().length;
      }
      if (!skinModelSkins.isEmpty()) {
        loadedSkins.put(skinModel, skinModelSkins);
      }
    }

    log.info("{} Loaded {} server skins with {} bytes.", LOG_PREFIX, skinCount, totalSize);
    return loadedSkins;
  }

  private static List<Path> findSkinFiles(Path skinModelFolder) {
    if (!Files.isDirectory(skinModelFolder, LinkOption.NOFOLLOW_LINKS)) {
      return List.of();
    }

    try (Stream<Path> skinPaths = Files.walk(skinModelFolder, MAX_FOLDER_DEPTH)) {
      return skinPaths
          .filter(skinPath -> Files.isRegularFile(skinPath, LinkOption.NOFOLLOW_LINKS))
          .filter(skinPath -> isSkinFileName(skinPath.getFileName().toString()))
          .sorted()
          .toList();
    } catch (IOException | UncheckedIOException e) {
      log.error("{} Unable to list skin files in {}:", LOG_PREFIX, skinModelFolder, e);
      return List.of();
    }
  }

  private static boolean isSkinFileName(String fileName) {
    return fileName.endsWith(SkinFileNameUtils.PNG_FILE_EXTENSION)
        && !fileName.endsWith(CustomSkinDataFiles.TEMPLATE_SUFFIX);
  }

  private static ServerSkin readServerSkin(SkinModel skinModel, Path skinFile) {
    String fileName = skinFile.getFileName().toString();
    UUID skinUUID = SkinFileNameUtils.getSkinUUID(fileName);
    try {
      if (Files.size(skinFile) > PngFileValidator.MAX_FILE_SIZE) {
        log.warn(
            "{} Ignoring {}, skin files are limited to {} bytes.",
            LOG_PREFIX,
            skinFile,
            PngFileValidator.MAX_FILE_SIZE);
        return null;
      }

      PngFileValidator.Result validationResult =
          PngFileValidator.sanitize(Files.readAllBytes(skinFile));
      if (!validationResult.isValid()) {
        log.warn("{} Ignoring {}: {}", LOG_PREFIX, skinFile, validationResult.rejectionReason());
        return null;
      }

      byte[] pngBytes =
          PngCompressionUtils.recompress(
              validationResult.pngBytes(), CustomSkinConfig.SERVER_SKIN_COMPRESSION_LEVEL);
      if (pngBytes.length > MAX_SKIN_FILE_SIZE) {
        log.warn(
            "{} Ignoring {}, server skins are limited to {} bytes after compression but it has {}"
                + " bytes.",
            LOG_PREFIX,
            skinFile,
            MAX_SKIN_FILE_SIZE,
            pngBytes.length);
        return null;
      }

      return new ServerSkin(skinUUID, skinModel, getDisplayName(fileName), pngBytes);
    } catch (IOException e) {
      log.error("{} Unable to read skin file {}:", LOG_PREFIX, skinFile, e);
      return null;
    }
  }

  static String getDisplayName(String fileName) {
    String displayName =
        UNSAFE_DISPLAY_NAME_CHARACTERS
            .matcher(
                fileName.substring(
                    0, fileName.length() - SkinFileNameUtils.PNG_FILE_EXTENSION.length()))
            .replaceAll("")
            .strip();
    return displayName.length() > MAX_DISPLAY_NAME_LENGTH
        ? displayName.substring(0, MAX_DISPLAY_NAME_LENGTH)
        : displayName;
  }

  public record ServerSkin(
      UUID skinUUID, SkinModel skinModel, String displayName, byte[] pngBytes) {}
}
